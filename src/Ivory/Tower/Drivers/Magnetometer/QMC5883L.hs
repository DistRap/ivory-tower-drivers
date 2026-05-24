{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Drivers.Magnetometer.QMC5883L
  ( QMC5883Config(..)
  , qmc5883DefaultConfig
  , qmc5883DefaultAddr
  , qmc5883lTower
  , OutputRate(..)
  , FullScale(..)
  , OversampleRatio(..)
  ) where

import Data.Word
import Ivory.BSP.STM32.Driver.I2C
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.Drivers.Magnetometer.QMC5883L.Regs
import Ivory.Tower.Drivers.Magnetometer.Types

data QMC5883Config =
  QMC5883Config
    { qmc5883ConfigScale      :: FullScale
    , qmc5883ConfigRate       :: OutputRate
    , qmc5883ConfigOversample :: OversampleRatio
    }
    deriving (Eq, Show)

qmc5883DefaultConfig :: QMC5883Config
qmc5883DefaultConfig =
  QMC5883Config
    { qmc5883ConfigScale      = Scale2Gauss
    , qmc5883ConfigRate       = Rate200Hz
    , qmc5883ConfigOversample = Oversample512
    }

qmc5883DefaultAddr :: I2CDeviceAddr
qmc5883DefaultAddr = I2CDeviceAddr 0x0D

qmc5883lTower
  :: BackpressureTransmit
       (Struct "i2c_transaction_request")
       (Struct "i2c_transaction_result")
  -> ChanOutput (Stored ITime)
  -> I2CDeviceAddr
  -> QMC5883Config
  -> Tower e
      (BackpressureTransmit
        (Stored ITime)
        (Struct "magnetometer_sample")
      )
qmc5883lTower (BackpressureTransmit req_chan res_chan) init_chan addr QMC5883Config{..} = do
  towerModule magnetometerTypes
  towerDepends magnetometerTypes
  (triggerChanIn, triggerChanOut) <- channel
  (sensorChanIn, sensorChanOut) <- channel

  let
    toMicroTesla raw = case qmc5883ConfigScale of
      Scale2Gauss -> raw / 120
      Scale8Gauss -> raw / 30

  monitor (named "SensorManager") $ do
    init_requests_area <- do
      let reqs = iarray
            [ regWriteInit addr Ctrl1 $ ctrl1Val Continuous qmc5883ConfigRate qmc5883ConfigScale qmc5883ConfigOversample
            , regWriteInit addr SetResetPeriod 0x01
            ] :: Init ('Array 2 ('Struct "i2c_transaction_request"))
      constArea <$> fmap showUnique (freshname "hmc5883l_init_requests") <*> pure reqs
    monitorModuleDef $ defConstMemArea init_requests_area
    let init_requests = addrOf init_requests_area

    initialized <- stateInit (named "Initialized") (ival false)
    active      <- stateInit (named "Active")      (ival false)
    s           <- state (named "Sample")

    coroutineHandler init_chan res_chan (named "Coroutine") $ do
      req_e <- emitter req_chan 1
      sens_e <- emitter sensorChanIn 1
      return $ CoroutineBody $ \yield -> do
        comment "entry to qmc5883l coroutine"
        forever $ do
          store (s ~> initfail) false
          arrayMap $ \ i -> do
            emit req_e $ init_requests ! i
            res <- yield
            code <- deref (res ~> resultcode)
            -- Set the initfail field if i2c failed
            when (code >? 0) (store (s ~> initfail) true)
          failed <- deref (s ~> initfail)
          unless failed breakOut
        store initialized true
        comment "finished initializing in qmc5883l coroutine"

        forever $ do
          -- Request originates from handler below
          setup_read_result <- yield
          comment "response received from periodic read"
          rc <- deref (setup_read_result ~> resultcode)
          -- Reset the samplefail field
          store (s ~> samplefail) (rc >? 0)
          -- Send request to perform read
          do_read_req <- fmap constRef $ local $ istruct
            [ tx_addr .= ival addr
            , tx_buf  .= iarray []
            , tx_len  .= ival 0
            , rx_len  .= ival 6
            ]
          emit req_e do_read_req
          res <- yield
          comment "response received from perform read request"
          store active false
          -- Unpack read, updating samplefail if failed.
          rc2 <- deref (res ~> resultcode)
          when (rc2 >? 0) (store (s ~> samplefail) true)
          payloads16 res 1 0 >>= store (s ~> x) . toMicroTesla . safeCast -- xl, xh
          payloads16 res 3 2 >>= store (s ~> y) . toMicroTesla . safeCast -- yl, yh
          payloads16 res 5 4 >>= store (s ~> z) . toMicroTesla . safeCast -- zl, zh
          -- Send the sample upstream.
          emit sens_e (constRef s)

    handler triggerChanOut (named "PeriodicRead") $ do
      req_e <- emitter req_chan 1
      callback $ const $ do
        i <- deref initialized
        a <- deref active
        when (i .&& iNot a) $ do
          -- Initiate a read
          setup_read_req <- fmap constRef $ local $ istruct
            [ tx_addr .= ival addr
            , tx_buf  .= iarray [ ival (fromIntegral (regAddr OutXL)) ]
            , tx_len  .= ival 1
            , rx_len  .= ival 0
            ]
          store active true
          emit req_e setup_read_req

  pure
    $ BackpressureTransmit
        triggerChanIn
        sensorChanOut
  where
  payloads16
    :: Ref s ('Struct "i2c_transaction_result")
    -> Ix 128
    -> Ix 128
    -> Ivory eff Sint16
  payloads16 res ixhi ixlo = do
    hi <- deref ((res ~> rx_buf) ! ixhi)
    lo <- deref ((res ~> rx_buf) ! ixlo)
    assign $ twosComplementCast ((safeCast hi `iShiftL` 8) .| safeCast lo)

  named :: String -> String
  named nm = "qmc5883l" ++ nm

regWriteInit
  :: I2CDeviceAddr
  -> Reg
  -> Word8
  -> Init ('Struct "i2c_transaction_request")
regWriteInit addr r v = istruct
  [ tx_addr .= ival addr
  , tx_buf  .= iarray [ ival (fromIntegral (regAddr r)), ival (fromIntegral v) ]
  , tx_len  .= ival 2
  , rx_len  .= ival 0
  ]
