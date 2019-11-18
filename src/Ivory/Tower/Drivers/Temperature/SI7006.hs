{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

-- SI7006 Temperature / relativy humidity I2C driver

module Ivory.Tower.Drivers.Temperature.SI7006 where

import Control.Monad (forM_, void)

import Ivory.Language
import Ivory.Stdlib

import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.Bus.I2C
import Ivory.Tower.HAL.Bus.I2C.DeviceAddr
import Ivory.Tower.HAL.Bus.Sched

import Ivory.Tower.Drivers.Temperature.Types
import Ivory.Tower.Drivers.Temperature.SI7006.Regs
import Ivory.Tower.Drivers.Temperature.SI7006.RegTypes
import Ivory.Tower.Drivers.Temperature.SI7006.Peripheral

named :: String -> String
named nm = "si7006_" ++ nm

si7006DefaultAddr :: I2CDeviceAddr
si7006DefaultAddr = I2CDeviceAddr 0x40

si7006TypesModule :: Module
si7006TypesModule = package "si7006_types" $ do
  defStruct (Proxy :: Proxy "sample_th")

-- Temperatue and humidity readout
--
-- (BackpressureTransmit reqChan, resChan) <- si7006Tower i2cTransmit initChan si7006DefaultAddr
-- To trigger sampling use reqChan, receive "sample_th" response on resChan
si7006Tower :: (IvoryArea a, IvoryZero a)
            => BackpressureTransmit ('Struct "i2c_transaction_request") ('Struct "i2c_transaction_result")
            -> ChanOutput ('Stored ITime)
            -> I2CDeviceAddr
            -> Tower e (BackpressureTransmit a ('Struct "sample_th"))
si7006Tower (BackpressureTransmit reqChan resChan) initChan addr = do
  towerModule si7006TypesModule
  towerDepends si7006TypesModule

  (sampleIn, sampleOut) <- channel
  (triggerIn, triggerOut) <- channel

  monitor (named "monitor") $ do
    last_sample <- state (named "sample")

    coroutineHandler initChan resChan (named "coroutine") $ do
      reqE <- emitter reqChan 1
      sampleE <- emitter sampleIn 1
      return $ CoroutineBody $ \yield -> do

        let rpc req = req >>= emit reqE >> yield
            readUnsigned16 req = do
              res <- rpc req
              code <- deref (res ~> resultcode)
              assert (code ==? 0)
              msb <- deref ((res ~> rx_buf) ! 0)
              lsb <- deref ((res ~> rx_buf) ! 1)
              -- compare crc of rx_buf indexes 0 & 1 with rx_buf ! 2
              crcAssert res [0, 1] 2
              return $ ((safeCast :: Uint8 -> Uint16) lsb) .| ((safeCast msb) `iShiftL` 8)

        forever $ do
          whoami <- noBreak $ rpc $ readDevReg addr (siFwRevision si7006)
          rc <- whoami ~>* resultcode
          when (rc ==? 0) $ do

            devId <- noBreak $ rpc $ readDevReg addr (siIDLow si7006)
            -- compare crc of rx_buf indexes 0, 2, 4, 6 with rx_buf ! 7
            crcAssert devId [0, 2, 4, 6] 7

            actual <- deref ((whoami ~> rx_buf) ! 0)
            when (actual ==? 0x20 .|| actual ==? 0xFF) $ breakOut

        forever $ noBreak $ do
          _ <- yield

          rhRaw <- readUnsigned16 $ readDevReg addr (siHumi si7006)
          humi <- assign $ (((safeCast :: Uint16 -> IFloat) rhRaw) * 125) / 2**16 - 6

          -- we could use siPrevTemp to get previous temperature measurement
          -- taken during humidity measurement but we won't get crc
          tRaw <- readUnsigned16 $ readDevReg addr (siTemp si7006)
          temp <- assign $ (((safeCast :: Uint16 -> IFloat) tRaw) * 175.72) / 2**16 - 46.85

          t <- getTime

          res <- local $ istruct [
              sample_th_temperature .= ival temp
            , sample_th_humidity    .= ival humi
            , sample_th_time        .= ival t
            ]

          emit sampleE (constRef res)
          refCopy last_sample res

    handler triggerOut (named "periodic_readout") $ do
      reqE <- emitter reqChan 1
      callback $ const $ do
        dummy <- readDevReg addr (siFwRevision si7006)
        emit reqE dummy

  return $ BackpressureTransmit triggerIn sampleOut

-- Temperatue and humidity readout with heater control channel
si7006TowerHeater :: (IvoryArea a, IvoryZero a)
                  => BackpressureTransmit ('Struct "i2c_transaction_request") ('Struct "i2c_transaction_result")
                  -> ChanOutput ('Stored ITime)
                  -> I2CDeviceAddr
                  -> Tower e ( BackpressureTransmit a ('Struct "sample_th")
                             , ChanInput ('Stored Uint8))
si7006TowerHeater i2cTransmit initChan addr = do
  (readTask, readReq) <- task "readout"
  sampleOut <- si7006Tower readReq initChan addr

  (heaterTask, heaterReq) <- task "heater"
  heaterIn <- si7006TowerHeaterOnly heaterReq initChan addr

  schedule (named "scheduler")
    [readTask, heaterTask] initChan i2cTransmit

  return (sampleOut, heaterIn)


-- Heater control only
si7006TowerHeaterOnly :: BackpressureTransmit ('Struct "i2c_transaction_request") ('Struct "i2c_transaction_result")
                      -> ChanOutput ('Stored ITime)
                      -> I2CDeviceAddr
                      -> Tower e (ChanInput ('Stored Uint8))
si7006TowerHeaterOnly (BackpressureTransmit reqChan resChan) initChan addr = do
  (heaterIn, heaterOut) <- channel

  monitor (named "monitor") $ do
    current <- state (named "current")

    coroutineHandler initChan resChan (named "coroutine") $ do
      reqE <- emitter reqChan 1
      return $ CoroutineBody $ \yield -> do

        let rpc req = req >>= emit reqE >> yield

        forever $ do
          whoami <- noBreak $ rpc $ readDevReg addr (siFwRevision si7006)
          rc <- whoami ~>* resultcode
          when (rc ==? 0) $ do
            actual <- deref ((whoami ~> rx_buf) ! 0)
            when (actual ==? 0x20 .|| actual ==? 0xFF) $ breakOut

        forever $ noBreak $ do
          _ <- yield
          hc <- deref current
          when (hc >? 0) $ do
            userData <- assign $ repToBits $ withBits 0 $ do
              setField heater heaterOn
            void $ rpc $ writeDevReg addr (siUser si7006) userData

            heaterData <- assign $ repToBits $ withBits 0 $ do
              setField heaterCurrent (fromRep hc)
            void $ rpc $ writeDevReg addr (siHeater si7006) heaterData

          when (hc ==? 0) $ do
            userData <- assign $ repToBits $ withBits 0 $ do
              setField heater heaterOff
            void $ rpc $ writeDevReg addr (siUser si7006) userData

    handler heaterOut (named "heater_control") $ do
      reqE <- emitter reqChan 1
      callback $ \x -> do
        refCopy current x
        dummy <- readDevReg addr (siFwRevision si7006)
        emit reqE dummy

  return heaterIn

crcUpdate :: (GetAlloc eff ~ 'Scope s)
          => Ref s2 ('Stored Uint8)
          -> Uint8
          -> Ivory eff ()
crcUpdate ref input = do
  current <- deref ref
  inbyte <- local $ ival input
  cur    <- local $ ival current

  upTo (0 :: Ix 9) 7 $ \_ix -> do
    inb <- deref inbyte
    c <- deref cur

    carry <- assign $ (c .^ inb) .& 0x80

    store cur $ c `iShiftL` 1
    when (carry ==? 0x80) $ do
      zc <- deref cur
      store cur $ zc .^ 0x31

    store inbyte $ inb `iShiftL` 1

  refCopy ref cur

crcAssert :: (GetAlloc eff ~ 'Scope s) 
          => Ref s2 ('Struct "i2c_transaction_result") -> [Ix 128] -> Ix 128 -> Ivory eff ()
crcAssert res inputIndexes expectedIx = do
  crc <- local $ ival 0
  forM_ inputIndexes $ \(i :: Ix 128) -> do
    x <- deref ((res ~> rx_buf) ! i)
    crcUpdate crc x
  out <- deref crc
  ex <- deref ((res ~> rx_buf) ! expectedIx)
  assert $ out ==? ex

readDevReg :: (GetAlloc eff ~ 'Scope s)
           => I2CDeviceAddr
           -> I2CReg a
           -> Ivory eff (ConstRef ('Stack s) ('Struct "i2c_transaction_request"))
readDevReg addr reg = fmap constRef $ local $ istruct
  [ tx_addr .= ival addr
  , tx_buf  .= iarray (map (ival . fromIntegral) (regRead reg))
  , tx_len  .= ival (fromIntegral $ length $ regRead reg)
  , rx_len  .= ival (fromIntegral $ regBytes reg)
  ]

writeDevReg :: (GetAlloc eff ~ 'Scope s)
            => I2CDeviceAddr
            -> I2CReg a
            -> Bits 8
            -> Ivory eff (ConstRef ('Stack s) ('Struct "i2c_transaction_request"))
writeDevReg addr reg dat = fmap constRef $ local $ istruct
  [ tx_addr .= ival addr
  , tx_buf  .= iarray (
      (map (ival . fromIntegral) (regWrite reg))
      ++ [ival $ toRep $ dat])
  , tx_len  .= ival (fromIntegral $ (length $ regWrite reg) + regBytes reg)
  , rx_len  .= ival 0
  ]
