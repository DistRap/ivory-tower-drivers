{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Ivory.Tower.Drivers.Pressure.BMP180
  ( bmp180Tower
  , Oversample(..)
  , module Ivory.Tower.Drivers.Pressure.BMP180.Types
  , defaultPressureAtSeaLevel
  , pressureToAltitude
  , seaLevelPressureForAltitude
  ) where

import Data.Word
import Ivory.BSP.STM32.Driver.I2C
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.Drivers.Pressure.BMP180.Regs
import Ivory.Tower.Drivers.Pressure.BMP180.Types

bmp180Addr :: I2CDeviceAddr
bmp180Addr = I2CDeviceAddr 0x77

defaultPressureAtSeaLevel :: IFloat
defaultPressureAtSeaLevel = 101325

-- | Get altitude in meters from
-- pressure at sea level and measured pressure
pressureToAltitude
  :: IFloat -- ^ Pressure at sea level
  -> IFloat -- ^ Measured pressure
  -> IFloat
pressureToAltitude p0 p = 44330 * (1 - (p / p0) ** (1/5.255))

-- | Get sea level pressure based on measured
-- atmospheric pressure and given altitude
seaLevelPressureForAltitude
  :: IFloat -- ^ Atmospheric pressure
  -> IFloat -- ^ Altitude
  -> IFloat
seaLevelPressureForAltitude p alt = p / (1 - (alt / 44330)) ** 5.255

-- | BMP180 temperature and pressure sensor driver
bmp180Tower
  :: BackpressureTransmit
       (Struct "i2c_transaction_request")
       (Struct "i2c_transaction_result")
  -> ChanOutput (Stored ITime)
  -> Oversample
  -> Tower e
      (ChanOutput (Struct "bmp_sample"))
bmp180Tower (BackpressureTransmit reqChan resChan) initChan oversample = do
  towerModule bmp180Types
  towerDepends bmp180Types
  (sensorChanIn, sensorChanOut) <- channel

  -- NB: see pressureWaitNumPeriods before changing
  per <- period (Milliseconds 5)

  monitor (named "SensorManager") $ do
    i2cReady    <- stateInit (named "I2CReady")    $ ival false
    initialized <- stateInit (named "Initialized") $ ival false
    s           <- stateInit (named "State")       $ ival bmpStateInit
    sample      <- state (named "Sample")

    tRaw        <- state (named "TRaw")
    pRaw        <- state (named "PRaw")

    calAC1 <- state (named "CalAC1")
    calAC2 <- state (named "CalAC2")
    calAC3 <- state (named "CalAC3")
    calAC4 <- state (named "CalAC4")
    calAC5 <- state (named "CalAC5")
    calAC6 <- state (named "CalAC6")
    calB1  <- state (named "CalB1")
    calB2  <- state (named "CalB2")
    calMB  <- state (named "CalMB")
    calMC  <- state (named "CalMC")
    calMD  <- state (named "CalMD")

    -- computed in temperature pass
    calB5  <- state (named "CalB5")

    handler initChan (named "Init") $ do
      callback $ const $ store i2cReady true

    handler resChan (named "Result") $ do
      sensE <- emitter sensorChanIn 1

      callback $ \res -> do
        resCode <- deref (res ~> resultcode)
        st <- deref s
        cond_
          [ st ==? bmpStateInit .&& resCode ==? 0 ==> do
              store initialized true
              store s bmpStateCalib

          , st ==? bmpStateCalib ==> do
              store s bmpStateTemperatureRequest

              payloads16S res  0  1 >>= store calAC1
              payloads16S res  2  3 >>= store calAC2
              payloads16S res  4  5 >>= store calAC3
              payloads16U res  6  7 >>= store calAC4
              payloads16U res  8  9 >>= store calAC5
              payloads16U res 10 11 >>= store calAC6
              payloads16S res 12 13 >>= store calB1
              payloads16S res 14 15 >>= store calB2
              payloads16S res 16 17 >>= store calMB
              payloads16S res 18 19 >>= store calMC
              payloads16S res 20 21 >>= store calMD

          , st ==? bmpStateTemperatureRequest ==> do
              store s bmpStateTemperature

          , st ==? bmpStateTemperature ==> do
              store s bmpStatePressureRequest
              payloads16S res 0 1 >>= store tRaw . (safeCast :: Sint16 -> Sint32)

              ut  <- deref tRaw
              ac5 <- safeCast <$> deref calAC5
              ac6 <- safeCast <$> deref calAC6
              mc  <- safeCast <$> deref calMC
              md  <- safeCast <$> deref calMD

              x1 <- assign $ ((ut - ac6) * ac5) `iDiv` pow2 15
              x2 <- assign $ (mc * pow2 11) `iDiv` (x1 + md)
              b5 <- assign $ x1 + x2
              t  <- assign $ (b5 + 8) `iDiv` 16

              store calB5 b5
              store
                (sample ~> bmp_sample_temperature)
                $ safeCast @Sint32 @IFloat t / 10

          , st ==? bmpStatePressureRequest ==> do
              store s bmpStatePressure

          , st ==? bmpStatePressure ==> do
              store s bmpStateTemperatureRequest

              let oss = fromIntegral $ osVal oversample

              payloads24 res 0 1 2 oss >>= store pRaw
              up <- deref pRaw

              ac1 <- safeCast <$> deref calAC1
              ac2 <- safeCast <$> deref calAC2
              ac3 <- safeCast <$> deref calAC3
              ac4 <- deref calAC4
              b1  <- safeCast <$> deref calB1
              b2  <- safeCast <$> deref calB2
              b5  <- deref calB5

              -- travel back to 8 bit era
              b6 <- assign $ b5 - 4000
              x1 <- assign $ (b2 * ((b6 * b6) `iDiv` pow2 12)) `iDiv` pow2 11
              x2 <- assign $ (ac2 * b6) `iDiv` pow2 11
              x3 <- assign $ x1 + x2

              -- cry
              b3 <- assign $ ((signCast @Sint32 @Uint32 (ac1 * 4 + x3) `iShiftL` oss) + 2) `iDiv` 4
              x1' <- assign $ (ac3 * b6) `iDiv` pow2 13
              x2' <- assign $ (b1 * ((b6 * b6) `iDiv` pow2 12)) `iDiv` pow2 16
              x3' <- assign $ ((x1' + x2') + 2) `iDiv` 4

              -- cry some more
              b4 <- assign $ ((safeCast @Uint16 @Uint32 ac4) * (signCast @Sint32 @Uint32 (x3' + 32768))) `iDiv` pow2 15
              b7 <- assign $ (signCast @Sint32 @Uint32 up - b3) * (50_000 `iShiftR` oss)
              p  <- assign $ (b7 <? 0x80_000_000) ? ((b7 * 2) `iDiv` b4, (b7 `iDiv` b4) * 2)

              -- here we upgrade to FPU (even the datasheet sample values are wrong when using ints)
              fp <- assign $ safeCast @Uint32 @IFloat p

              x1''  <- assign $ (fp / pow2 8) * (fp / pow2 8)
              x1''' <- assign $ (x1'' * 3038) / pow2 16
              x2''  <- assign $ ((-7357) * fp) / pow2 16

              store
                (sample ~> bmp_sample_pressure)
                $ fp + (x1''' + x2'' + 3791) / 16

              emit
                sensE
                $ constRef sample
          ]

    counter <- stateInit (named "Counter") (ival @Uint8 0)

    handler per (named "Period") $ do
      reqE <- emitter reqChan 1
      callback $ const $ do
        i2cOk <- deref i2cReady

        when i2cOk $ do
          st <- deref s
          count <- deref counter

          cond_
            [ st ==? bmpStateInit ==> do
                req <- fmap constRef $ local $ istruct
                  [ tx_addr .= ival bmp180Addr
                  , tx_buf  .= iarray [ ival $ fromIntegral $ regAddr Ident ]
                  , tx_len  .= ival 1
                  , rx_len  .= ival 1
                  ]
                emit reqE req

            , st ==? bmpStateCalib ==> do
                req <- fmap constRef $ local $ istruct
                  [ tx_addr .= ival bmp180Addr
                  , tx_buf  .= iarray [ ival $ fromIntegral $ regAddr Calib ]
                  , tx_len  .= ival 1
                  , rx_len  .= ival 22
                  ]
                emit reqE req

            , st ==? bmpStateTemperatureRequest ==> do
                req <-
                    fmap
                      constRef
                  $ local
                  $ regWriteInit
                      Ctrl
                      $ ctrlVal
                          MeasurementMode_Temperature
                          Oversample1 -- ignored for temperature meas

                emit reqE req
                store counter 0

            , st ==? bmpStateTemperature .&& count ==? 1 ==> do
                req <- fmap constRef $ local $ istruct
                  [ tx_addr .= ival bmp180Addr
                  , tx_buf  .= iarray [ ival $ fromIntegral $ regAddr Out ]
                  , tx_len  .= ival 1
                  , rx_len  .= ival 3
                  ]
                emit reqE req
                store counter 0

            , st ==? bmpStatePressureRequest ==> do
                req <-
                    fmap
                      constRef
                  $ local
                  $ regWriteInit
                      Ctrl
                      $ ctrlVal
                          MeasurementMode_Pressure
                          oversample

                emit reqE req
                store counter 0

            , st ==? bmpStatePressure .&& count ==? pressureWaitNumPeriods oversample ==> do
                req <- fmap constRef $ local $ istruct
                  [ tx_addr .= ival bmp180Addr
                  , tx_buf  .= iarray [ ival $ fromIntegral $ regAddr Out ]
                  , tx_len  .= ival 1
                  , rx_len  .= ival 3
                  ]
                emit reqE req
                store counter 0
            ]

          counter += 1

  pure sensorChanOut

  where
  -- Temperature: trigger read -> 4.5 ms delay -> readOut
  -- Pressure: trigger read -> 4.5 up to 25.5ms delay based on oversampling
  -- our period must be 5ms (or larger, or better algo!)
  pressureWaitNumPeriods = \case
    Oversample1 -> 1
    Oversample2 -> 2
    Oversample4 -> 3
    Oversample8 -> 6

  pow2 ex = 2^(ex :: Int)

  payloads16S
    :: ConstRef s ('Struct "i2c_transaction_result")
    -> Ix 128
    -> Ix 128
    -> Ivory eff Sint16
  payloads16S res ixhi ixlo = do
    hi <- deref ((res ~> rx_buf) ! ixhi)
    lo <- deref ((res ~> rx_buf) ! ixlo)
    assign
      $ twosComplementCast
      $ (safeCast hi `iShiftL` 8) .| safeCast lo

  payloads16U
    :: ConstRef s ('Struct "i2c_transaction_result")
    -> Ix 128
    -> Ix 128
    -> Ivory eff Uint16
  payloads16U res ixhi ixlo = do
    hi <- deref ((res ~> rx_buf) ! ixhi)
    lo <- deref ((res ~> rx_buf) ! ixlo)
    assign
      $ (safeCast hi `iShiftL` 8) .| safeCast lo

  payloads24
    :: ConstRef s ('Struct "i2c_transaction_result")
    -> Ix 128
    -> Ix 128
    -> Ix 128
    -> Uint32 -- ^ oss
    -> Ivory eff Sint32
  payloads24 res ixhi ixmi ixlo oss = do
    hi <- deref ((res ~> rx_buf) ! ixhi)
    mi <- deref ((res ~> rx_buf) ! ixmi)
    lo <- deref ((res ~> rx_buf) ! ixlo)
    assign
      $ twosComplementCast
      $ (  (safeCast hi `iShiftL` 16)
        .| (safeCast mi `iShiftL` 8)
        .| safeCast lo
        ) `iShiftR` (8 - oss)

  named :: String -> String
  named nm = "bmp180" ++ nm

newtype BMPState = BMPState Uint8
  deriving (IvoryType, IvoryVar, IvoryExpr, IvoryEq, IvoryStore, IvoryInit, IvoryZeroVal)

bmpStateInit
  , bmpStateCalib
  , bmpStateTemperatureRequest
  , bmpStateTemperature
  , bmpStatePressureRequest
  , bmpStatePressure
  :: BMPState

[   bmpStateInit
  , bmpStateCalib
  , bmpStateTemperatureRequest
  , bmpStateTemperature
  , bmpStatePressureRequest
  , bmpStatePressure
  ] = map (BMPState . fromInteger) [0..5]

regWriteInit
  :: Reg
  -> Word8
  -> Init ('Struct "i2c_transaction_request")
regWriteInit r v = istruct
  [ tx_addr .= ival bmp180Addr
  , tx_buf  .= iarray [ ival (fromIntegral (regAddr r)), ival (fromIntegral v) ]
  , tx_len  .= ival 2
  , rx_len  .= ival 0
  ]
