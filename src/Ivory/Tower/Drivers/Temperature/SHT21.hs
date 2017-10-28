{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Drivers.Temperature.SHT21 (sht21Tower) where

import Ivory.BSP.STM32.Driver.I2C

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface

aModule :: Module
aModule = package "MyPackage" $ do
  incl sht21decodeTemp
  incl sht21decodeHum

status_bits_mask :: Uint16
status_bits_mask = 0xFFFC

sht21decodeTemp :: Def('[ConstRef s ('Array 128 ('Stored Uint8))]' :-> IFloat)
sht21decodeTemp = proc "sht21decodeTemp" $ \rxbuf -> body $ do
  -- TODO CRC calculation - signal failure by returning NaN?
  rx0 <- deref (rxbuf ! 0)
  rx1 <- deref (rxbuf ! 1)
  i <- assign $ (.& status_bits_mask) $ ((safeCast rx0 :: Uint16) `iShiftL` 8) + (safeCast rx1 :: Uint16)
  res <- assign $ ((safeCast i :: IFloat) / 2**16) * 175.72 - 46.85
  ret res

sht21decodeHum :: Def('[ConstRef s ('Array 128 ('Stored Uint8))]' :-> IFloat)
sht21decodeHum = proc "sht21decodeHum" $ \rxbuf -> body $ do
  -- TODO CRC calculation
  rx0 <- deref (rxbuf ! 0)
  rx1 <- deref (rxbuf ! 1)
  i <- assign $ (.& status_bits_mask) $ ((safeCast rx0 :: Uint16) `iShiftL` 8) + (safeCast rx1 :: Uint16)
  res <- assign $ ((safeCast i :: IFloat) / 2**16) * 125.0 - 6.0
  ret res

sht21Tower :: BackpressureTransmit ('Struct "i2c_transaction_request") ('Struct "i2c_transaction_result")
           -> Tower e (ChanOutput ('Stored IFloat), ChanOutput ('Stored IFloat))
sht21Tower (BackpressureTransmit req res) = do -- XXX what about _ready?
  towerModule aModule
  towerDepends aModule
  (temp_in, temp_out) <- channel
  (hum_in, hum_out) <- channel
  periodic <- period (Milliseconds 200)
  monitor "sht21controller" $ do
    dbg <- state "dbg"
    what <- state "what" -- 0 -> read/send temperature, 1 -> humidity
    handler periodic "periodic" $ do
      req_emitter <- emitter req 1
      callback $ const $ do
        w <- deref what
        opcode <- local izero
        ifte_ (w ==? (0 :: Uint8)) (store opcode 0xE3) (store opcode 0xE5)
        opcode' <- deref opcode
        r <- local $ istruct
               [ tx_addr   .= ival sht21addr
               , tx_buf    .= iarray [ival opcode']
               , tx_len    .= ival 1
               , rx_len    .= ival 3
               ]
        emit req_emitter (constRef r)

    handler res "result" $ do
      temp_emitter <- emitter temp_in 1
      hum_emitter <- emitter hum_in 1
      callback $ \rref -> do
        w <- deref what
        rc <- deref (rref ~> resultcode)
        refCopy dbg rref
        when (rc ==? 0) $ do
          when (w ==? 0) $ do
            temp <- call sht21decodeTemp $ (rref ~> rx_buf)
            emitV temp_emitter temp
          unless (w ==? 0) $ do
            hum <- call sht21decodeHum $ (rref ~> rx_buf)
            emitV hum_emitter hum
        store what (w .^ 1)
  return (temp_out, hum_out)
  where
  sht21addr = I2CDeviceAddr 0x40
