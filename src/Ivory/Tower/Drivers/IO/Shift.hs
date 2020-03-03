{-# LANGUAGE DataKinds #-}
module Ivory.Tower.Drivers.IO.Shift where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.Bus.SPI
import Ivory.Tower.HAL.Bus.SPI.DeviceHandle

-- | 595 shift register over SPI "driver"
io595 :: BackpressureTransmit ('Struct "spi_transaction_request") status
      -> SPIDeviceHandle
      -> Tower e (ChanInput ('Stored Uint8))
io595 (BackpressureTransmit req_c _res_c) spiDev = do
  c <- channel
  monitor "595" $ do
    handler (snd c) "handle595" $ do
      req_e <- emitter req_c 1

      callbackV $ \val -> do
        x <- local $ istruct
          [ tx_device .= ival spiDev
          , tx_buf .= iarray [ival val]
          , tx_len .= ival 1 ]

        emit req_e $ constRef x

  return $ fst c
