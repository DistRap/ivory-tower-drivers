{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Drivers.IO.VNI8200
  ( vniTower
  , module Ivory.Tower.Drivers.IO.VNI8200.Types
  ) where

import Ivory.Language
import Ivory.HW.Module
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.Bus.SPI
import Ivory.Tower.HAL.Bus.SPI.DeviceHandle

import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.Base

import Ivory.Tower.Drivers.IO.VNI8200.Regs
import Ivory.Tower.Drivers.IO.VNI8200.Types

-- | VNI8200XP driver
--
-- Can trigger watchdog if `watchdogPin` is defined.
-- If it's `Nothing` there's some other source which triggers
-- it for us (for example reading CLT01-38SQZ chip like on PLC01A1)
--
-- Driver uses 16 bit mode with checksum.
vniTower
  :: BackpressureTransmit
       ('Struct "spi_transaction_request")
       ('Struct "spi_transaction_result")
  -> ChanOutput ('Stored ITime)
  -> SPIDeviceHandle
  -> GPIOPin
  -> Maybe GPIOPin
  -> Tower
       e
       ( ChanInput ('Array 8 (Stored IBool))
       , ChanOutput ('Struct "vni_status")
       )
vniTower (BackpressureTransmit req res) rdy spiDev enablePin watchdogPin = do
  towerModule vniTypes
  towerDepends vniTypes

  setOutputs <- channel
  outStatus <- channel

  -- output setting / polling period, arbitrary value
  p <- period (Milliseconds 35)

  -- watchdog period, only used if watchdogPin is Just pin
  pw <- period (Milliseconds 35)

  monitor "vni8200" $ do
    monitorModuleDef $ hw_moduledef

    outputs <- state "outputs"
    ready <- state "ready"

    handler rdy "spiReady" $ do
      callback $ const $ store ready true

    handler (snd setOutputs) "setOutputs" $ do
      callback $ refCopy outputs

    handler p "periodic" $ do
      reqE <- emitter req 1
      callback $ const $ do
        isReady <- deref ready
        when isReady $ do
          numericOutputs <- local $ izero
          arrayMap $ \ix -> do
            oval <- deref (outputs ! ix)
            when oval $ do
              prev <- deref numericOutputs
              store
                numericOutputs
                $ prev + 1 `iShiftL` (bitCast . (signCast :: Sint32 -> Uint32) . fromIx) ix

          out <- deref numericOutputs
          p0 <- xorBits (pure true) out
          -- even bits
          p1 <- xorBits (\x -> (fromIx x .% 2) /=? 0) out
          -- odd bits
          p2 <- xorBits (\x -> (fromIx x .% 2) ==? 0) out

          p0' <- deref p0
          p1' <- deref p1
          p2' <- deref p2
          -- negated p0
          p0N <- assign $ 1 - p0'
          crc <- assign $
              p0N
            + (p0' `iShiftL` 1)
            + (p1' `iShiftL` 2)
            + (p2' `iShiftL` 3)

          x <- local $ istruct
            [ tx_device .= ival spiDev
            , tx_buf .= iarray ( map ival [out, crc] )
            , tx_len .= ival 2 ]

          emit reqE $ constRef x

    handler res "spiResult" $ do
      outStatusE <- emitter (fst outStatus) 1
      callback $ \x -> do
        faults <- deref (x ~> rx_buf ! 0)
        status <- deref (x ~> rx_buf ! 1)
        statusReg <- assign $ fromRep status

        -- P0 = F0 + F1 + F2 + F3 + F4 + F5 + F6 + F7
        p0fs <- xorBits (pure true) faults
        p0 <- deref p0fs

        -- P1 = PC + FB_OK + F1 + F3 + F5 + F7
        p1fs <- xorBits (\i -> (fromIx i .% 2) /=? 0) faults
        p1fs' <- deref p1fs
        p1 <- assign
          $  p1fs'
          .^ (toRep $ statusReg #. vni_parity_fail)
          .^ (toRep $ statusReg #. vni_fb_ok)

        -- P2 = !PG + !TWARN + F0 + F2 + F4 + F6
        p2fs <- xorBits (\i -> (fromIx i .% 2) ==? 0) faults
        p2fs' <- deref p2fs
        p2 <- assign
          $ p2fs'
          .^ (toRep $ statusReg #. vni_not_power_good)
          .^ (toRep $ statusReg #. vni_temperature_warning)

        -- nP0 = !P0
        np0 <- assign $ 1 - p0

        vniStatus <- local $ istruct []
        store
          (vniStatus ~> vni_dc_dc_ok)
          (bitToBool (statusReg #. vni_fb_ok))

        store
          (vniStatus ~> vni_over_temperature_warning)
          (bitToBool (statusReg #. vni_temperature_warning))

        store
          (vniStatus ~> vni_power_not_good)
          (bitToBool (statusReg #. vni_not_power_good))

        store
          (vniStatus ~> vni_parity_error)
          (     toRep (statusReg #. vni_p0) /=? p0
            .&& toRep (statusReg #. vni_p1) /=? p1
            .&& toRep (statusReg #. vni_p2) /=? p2
            .&& toRep (statusReg #. vni_np0) /=? np0
            .|| bitToBool (statusReg #. vni_parity_fail)
          )

        arrayMap $ \ix -> do
          store
            (vniStatus ~> vni_channel_over_temperature ! ix)
            (1 ==?
              faults
              `iShiftR`
              (bitCast . (signCast :: Sint32 -> Uint32) . fromIx) ix
            )

        emit outStatusE (constRef vniStatus)

    handler systemInit "ini" $ do
      callback $ const $ do
        pinEnable enablePin
        pinSetMode enablePin gpio_mode_output
        pinSet enablePin

        case watchdogPin of
          Nothing -> return ()
          Just pin -> do
            pinEnable pin
            pinSetMode pin gpio_mode_output

    case watchdogPin of
      Nothing -> return ()
      Just pin -> do
        handler pw "watchdogPer" $ do
          callback $ const $ do
            pinClear pin
            pinSet pin

  pure
    ( fst setOutputs
    , snd outStatus
    )
