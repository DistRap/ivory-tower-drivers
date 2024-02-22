{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Drivers.IO.CLT01
  ( cltTower
  , module Ivory.Tower.Drivers.IO.CLT01.Types
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.Bus.SPI
import Ivory.Tower.HAL.Bus.SPI.DeviceHandle

import Ivory.Base (xnorBits, xnorBitRange)

import Ivory.Tower.Drivers.IO.CLT01.Regs
import Ivory.Tower.Drivers.IO.CLT01.Types

-- | CLT01-38SQ7 driver
cltTower
  :: BackpressureTransmit
       ('Struct "spi_transaction_request")
       ('Struct "spi_transaction_result")
  -> ChanOutput ('Stored ITime)
  -> SPIDeviceHandle
  -> Tower
       e
       (ChanOutput ('Struct "clt_status"))
cltTower (BackpressureTransmit req res) rdy spiDev = do
  towerModule cltTypes
  towerDepends cltTypes

  inStatus <- channel

  p <- period (Milliseconds 35) -- VNI watchdog period

  monitor "clt01" $ do
    ready <- state "ready"

    handler rdy "spiReady" $ do
      callback $ const $ store ready true

    handler p "periodic" $ do
      reqE <- emitter req 1
      callback $ const $ do
        isReady <- deref ready
        when isReady $ do
          x <- local $ istruct
            [ tx_device .= ival spiDev
            , tx_buf .= iarray ( map ival [0x0, 0x0] )
            , tx_len .= ival 2 ]

          emit reqE $ constRef x

    handler res "spiResult" $ do
      inStatusE <- emitter (fst inStatus) 1
      callback $ \x -> do
        ins <- deref (x ~> rx_buf ! 0)
        pc1 <- xnorBits ins
        pc2 <-  xnorBitRange 4 7 ins
        pc3 <-  xnorBitRange 0 3 ins
        pc4 <-  xnorBitRange 2 5 ins
        pc1' <- deref pc1
        pc2' <- deref pc2
        pc3' <- deref pc3
        pc4' <- deref pc4

        crc <- assign
          $ (pc1' `iShiftL` 3)
          + (pc2' `iShiftL` 2)
          + (pc3' `iShiftL` 1)
          + (pc4' `iShiftL` 0)

        status <- deref (x ~> rx_buf ! 1)
        statusReg <- assign $ fromRep status

        cltStatus <- local $ istruct []
        store
          (cltStatus ~> clt_under_voltage_alarm)
          (iNot $ bitToBool (statusReg #. clt_voltage_ok))

        store
          (cltStatus ~> clt_over_temperature_alarm)
          (iNot $ bitToBool (statusReg #. clt_temperature_ok))

        store
          (cltStatus ~> clt_parity_error)
          (toRep (statusReg #. clt_parity) /=? crc)

        arrayMap $ \ix -> do
          store
            (cltStatus ~> clt_channel_status ! ix)
            (1 ==?
              ins
              `iShiftR`
              (bitCast . (signCast :: Sint32 -> Uint32) . fromIx) ix
            )

        emit inStatusE (constRef cltStatus)

  pure $ snd inStatus
