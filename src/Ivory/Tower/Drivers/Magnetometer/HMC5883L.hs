-- Ported from smaccmpilot-stm32f4 ivory-px4-hw (BSD3)

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Drivers.Magnetometer.HMC5883L
  ( HMC5883Config(..)
  , hmc5883DefaultConfig
  , hmc5883DefaultAddr
  , hmc5883lTower
  , Gain(..)
  , OutputRate(..)
  , SampleAveraging(..)
  , BiasMode(..)
  ) where

import Data.Word
import Ivory.BSP.STM32.Driver.I2C
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.Drivers.Magnetometer.HMC5883L.Regs
import Ivory.Tower.Drivers.Magnetometer.Types

data HMC5883Config =
  HMC5883Config
    { hmc5883ConfigGain       :: Gain
    , hmc5883ConfigRate       :: OutputRate
    , hmc5883ConfigAveraging  :: SampleAveraging
    , hmc5883ConfigBias       :: BiasMode
    }
    deriving (Eq, Show)

hmc5883DefaultConfig :: HMC5883Config
hmc5883DefaultConfig =
  HMC5883Config
    { hmc5883ConfigGain       = LSBGauss1090
    , hmc5883ConfigRate       = Rate75hz
    , hmc5883ConfigAveraging  = Average8
    , hmc5883ConfigBias       = NoBias
    }

hmc5883DefaultAddr :: I2CDeviceAddr
hmc5883DefaultAddr = I2CDeviceAddr 0x1E

hmc5883lTower
  :: BackpressureTransmit
       (Struct "i2c_transaction_request")
       (Struct "i2c_transaction_result")
  -> ChanOutput (Stored ITime)
  -> I2CDeviceAddr
  -> HMC5883Config
  -> Tower e
      (BackpressureTransmit
        (Stored ITime)
        (Struct "magnetometer_sample")
      )
hmc5883lTower (BackpressureTransmit req_chan res_chan) init_chan addr HMC5883Config{..} = do
  towerModule magnetometerTypes
  towerDepends magnetometerTypes
  (triggerChanIn, triggerChanOut) <- channel
  (sensorChanIn, sensorChanOut) <- channel

  let
    toMicroTesla = (*gainScale hmc5883ConfigGain)

  monitor (named "SensorManager") $ do
    init_requests_area <- do
      let reqs = iarray
            [ regWriteInit addr ConfA $ confAVal hmc5883ConfigAveraging hmc5883ConfigRate hmc5883ConfigBias
            , regWriteInit addr ConfB $ confBVal hmc5883ConfigGain
            , regWriteInit addr Mode  $ modeVal  Continuous
            ] :: Init ('Array 3 ('Struct "i2c_transaction_request"))
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
        comment "entry to hmc5883l coroutine"
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
        comment "finished initializing in hmc5883l coroutine"

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
          payloads16 res 0 1 >>= store (s ~> x) . toMicroTesla . safeCast -- xh, xl
          payloads16 res 2 3 >>= store (s ~> z) . toMicroTesla . safeCast -- zh, zl
          payloads16 res 4 5 >>= store (s ~> y) . toMicroTesla . safeCast -- yh, yl
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
            , tx_buf  .= iarray [ ival (fromIntegral (regAddr OutXH)) ]
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
  named nm = "hmc5883l" ++ nm

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
