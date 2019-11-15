{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Ivory.Tower.Drivers.ADC.PCF8591 where

import Ivory.Language
import Ivory.Stdlib
import Ivory.HW
import Ivory.HW.BitData
import Ivory.HW.Reg

import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.Bus.I2C
import Ivory.Tower.HAL.Bus.I2C.DeviceAddr
import Ivory.Tower.HAL.Bus.Sched

pcf8591DefaultAddr :: I2CDeviceAddr
pcf8591DefaultAddr = I2CDeviceAddr 0x48

named :: String -> String
named nm = "pcf8591_" ++ nm

type ADCArray = 'Array 4 ('Stored Uint8)

-- | PCF8591 8bit DAC + 4 channel ADC driver
pcf8591Tower :: BackpressureTransmit ('Struct "i2c_transaction_request") ('Struct "i2c_transaction_result")
            -> ChanOutput ('Stored ITime)
            -> I2CDeviceAddr
            -> Tower e (BackpressureTransmit
                  ('Stored ITime)
                  ADCArray
                  --('Struct "pcf_sample")
                , ChanInput ('Stored Uint8))
pcf8591Tower i2cTransmit@(BackpressureTransmit reqChan resChan) initChan addr = do
  (adcTask, adcReq) <- task "adc"
  tADC <- pcf8591ADCTower adcReq initChan addr

  (dacTask, dacReq) <- task "dac"
  writeChan <- pcf8591DACTower dacReq initChan addr
  schedule (named "scheduler")
    [adcTask, dacTask] initChan i2cTransmit

  return (tADC, writeChan)

pcf8591ADCTower :: BackpressureTransmit ('Struct "i2c_transaction_request") ('Struct "i2c_transaction_result")
            -> ChanOutput ('Stored ITime)
            -> I2CDeviceAddr
            -> Tower e (BackpressureTransmit
                  ('Stored ITime)
                  ADCArray
                )
pcf8591ADCTower i2cTransmit@(BackpressureTransmit reqChan resChan) initChan addr = do
  readRequest <- channel
  readResponse <- channel

  monitor "pcf8591" $ do
    adcLast <- stateInit "adcLast" (iarray [])
    coroutineHandler (snd readRequest) resChan (named "read_response") $ do
      e <- emitter (fst readResponse) 1
      req <- emitter reqChan 1
      return $ CoroutineBody $ \yield -> do
        -- read all 4 ADCs in sequnce
        -- 0x40 - DAC output on
        -- 0x4  - autoincrement
        r <- local $ istruct
              [ tx_addr .= ival addr
              , tx_buf  .= iarray [ival 0x44]
              , tx_len  .= ival 1
              , rx_len  .= ival 0
              ]

        emit req (constRef r)

        _ <- yield
        r <- local $ istruct
              [ tx_addr .= ival addr
              , tx_buf  .= izero
              , tx_len  .= izero
              , rx_len  .= ival 5
              ]
        emit req (constRef r)

        x <- yield
        rc <- deref (x ~> resultcode)
        when (rc ==? 0) $ do
          -- skip first byte with previous reading
          c0 <- deref ((x ~> rx_buf) ! 1) -- channel 0
          c1 <- deref ((x ~> rx_buf) ! 2)
          c2 <- deref ((x ~> rx_buf) ! 3)
          c3 <- deref ((x ~> rx_buf) ! 4)

          arrayMap $ \i -> do
            val <- deref ((x ~> rx_buf) ! (toIx $ fromIx i + 1))
            store (adcLast ! i) val

          s <- local $ istruct [
              pcf_channel0 .= ival c0
            , pcf_channel1 .= ival c1
            , pcf_channel2 .= ival c2
            , pcf_channel3 .= ival c3
            ]
          emit e (constRef adcLast)

  return $ BackpressureTransmit
            (fst readRequest)
            (snd readResponse)

pcf8591DACTower :: BackpressureTransmit ('Struct "i2c_transaction_request") ('Struct "i2c_transaction_result")
            -> ChanOutput ('Stored ITime)
            -> I2CDeviceAddr
            -> Tower e (ChanInput ('Stored Uint8))
pcf8591DACTower i2cTransmit@(BackpressureTransmit reqChan resChan) initChan addr = do
  writeChan <- channel

  monitor "pcf8591" $ do
    handler (snd writeChan) (named "write") $ do
      e <- emitter reqChan 1
      callbackV $ \x -> do
        r <- local $ istruct
              [ tx_addr .= ival addr
              , tx_buf  .= iarray [ival 0x40, ival x]
              , tx_len  .= ival 2
              , rx_len  .= ival 0
              ]

        emit e (constRef r)

  return $ fst writeChan

