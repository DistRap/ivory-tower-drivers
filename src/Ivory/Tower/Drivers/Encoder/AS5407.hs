{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Ivory.Tower.Drivers.Encoder.AS5407 where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.Bus.SPI
import Ivory.Tower.HAL.Bus.SPI.DeviceHandle

import Ivory.Tower.Drivers.Encoder.AS5407.Regs
import Ivory.Tower.Drivers.Encoder.AS5407.Types

import Ivory.Base

-- build a AS5407 control message
as5407Msg :: Bit -> Bit -> Bits 14 -> AS5407Cmd
as5407Msg parity read addr = fromRep $ withBits 0 $ do
  setField as_read read
  setField as_parc parity
  setField as_addr addr

as5407Data :: Bit -> Bits 14 -> AS5407Data
as5407Data parity val = fromRep $ withBits 0 $ do
  setField as_pard parity
  setField as_data val

-- create an SPI request from device and AS5407 cmd
spiCmd :: (GetAlloc eff ~ 'Scope s)
        => SPIDeviceHandle
        -> AS5407Cmd
        -> Ivory eff (ConstRef ('Stack s) ('Struct "spi_transaction_request"))
spiCmd dev msg = fmap constRef $ local $ istruct
  [ tx_device .= ival dev
  , tx_buf    .= iarray [h msg, l msg]
  , tx_len    .= ival 2
  ]
  where l x = ival $ bitCast $ toRep x
        h x = ival $ bitCast $ (toRep x) `iShiftR` 8

spiData :: (GetAlloc eff ~ 'Scope s)
        => SPIDeviceHandle
        -> AS5407Data
        -> Ivory eff (ConstRef ('Stack s) ('Struct "spi_transaction_request"))
spiData dev msg = fmap constRef $ local $ istruct
  [ tx_device .= ival dev
  , tx_buf    .= iarray [h msg, l msg]
  , tx_len    .= ival 2
  ]
  where l x = ival $ bitCast $ toRep x
        h x = ival $ bitCast $ (toRep x) `iShiftR` 8

as5407 :: forall init e . (IvoryZero init, IvoryArea init)
       =>  BackpressureTransmit
             ('Struct "spi_transaction_request")
             ('Struct "spi_transaction_result")
       -> SPIDeviceHandle
       -> ChanOutput init
       -> Tower e ()
as5407 (BackpressureTransmit req_c res_c) dev initChan = do
  amsTowerDeps

  monitor "as5407" $ do

    txcmd <- state "txcmd"
    rxcmd <- state "rxcmd"
    txdata <- state "txdata"
    rxdata <- state "rxdata"

    txcmd2 <- state "txcmd2"
    rxcmd2 <- state "rxcmd2"
    txdata2 <- state "txdata2"
    rxdata2 <- state "rxdata2"

    diag <- state "asdiag"

    crc <- stateInit "crc" (ival (0 :: Uint16))
    dbg <- stateInit "dbg" (ival (0 :: Uint16))
    excrc <- stateInit "excrc" (ival false)

    cnt <- stateInit "ascnt" (ival (0 :: Uint8))

    coroutineHandler initChan res_c "as5407coro" $ do
      req_e <- emitter req_c 1
      return $ CoroutineBody $ \ yield -> do
        let rpc req = do
              newreq <- req
              emit req_e newreq
              return ()

        forever $ do
          --rpc $ spiReq dev (as5407Msg (fromRep 0x0) (fromRep 0x1) (fromRep 0x0018)) (as5407Data (fromRep 0x0) (fromRep 0x0))
          --newreq <- spiReq dev (as5407Msg (fromRep 0x0) (fromRep 0x1) (fromRep 0x0018)) (as5407Data (fromRep 0x0) (fromRep 0x0))
          --
          --
          --wat
          --newq <- spiReq dev (as5407Msg (fromRep 0x1) (fromRep 0x1) (fromRep 0x3FFC)) (fromRep 0xC000)-- (as5407Data (fromRep 0x0) (fromRep 0x0))
          --refCopy dbgread newq
          --emit req_e newq
          --x <- yield
          --refCopy dbg x

          --newreq <- spiReq dev (as5407Msg (fromRep 0x0) (fromRep 0x1) (fromRep 0x0001)) (as5407Data (fromRep 0x0) (fromRep 0x0))
          --refCopy dbgerrread newreq
          --emit req_e newreq
          --a <- yield
          --refCopy dbgerr a
          --

          -- XXX: convert as5407Msg to INTs and macro the shit out of it?
          cmd <- spiCmd dev (as5407Msg (fromRep 0x1) (fromRep 0x1) (fromRep 0x3FFC))
          refCopy txcmd cmd
          emit req_e cmd
          x <- yield
          refCopy rxcmd x

          dat <- spiData dev (fromRep 0xC000)-- (as5407Data (fromRep 0x0) (fromRep 0x0))

          refCopy txdata dat
          emit req_e dat
          x <- yield
          refCopy rxdata x

          hi <- deref ((x ~> rx_buf) ! 0)
          lo <- deref ((x ~> rx_buf) ! 1)
          u16dat <- assign $ (((safeCast hi `iShiftL` 8) .| safeCast lo) :: Uint16)

          diagFromReg u16dat diag


          store crc 0
          store excrc (crcFromReg u16dat)

          arrayMap $ \(ix :: Ix 13) -> do
            crc' <- deref crc
            store dbg (ixToU16 ix)
            store crc (crc' .^ ((u16dat `iShiftL` (13 - ixToU16 ix)) `iShiftR` (ixToU16 ix)))
            return ()

          cmd2 <- spiCmd dev (as5407Msg (fromRep 0x1) (fromRep 0x1) (fromRep 0x3FFF))
          refCopy txcmd2 cmd
          emit req_e cmd2
          x <- yield
          refCopy rxcmd2 x

          dat2 <- spiData dev (fromRep 0xC000)-- (as5407Data (fromRep 0x0) (fromRep 0x0))

          refCopy txdata2 dat2
          emit req_e dat2
          x <- yield
          refCopy rxdata2 x

          cnt += 1
          return ()

          --once onceStateVar $ ledOn redLED

          --once x act = do
          --  x' <- deref x 
          --  ifte_ (iNot x') (store x true >> act) ()
          --
          --  once for channels?
          --
          --  isRising channel?


  return ()

crcFromReg :: Uint16 -> IBool
crcFromReg x = bitToBool $ fromRep x #. as_pard

diagFromReg :: Uint16 -> Ref s ('Struct "ams_diag") -> Ivory eff ()
diagFromReg x r = do
  p magfield_low        as_diag_magl
  p magfield_high       as_diag_magh
  p cordic_overflow     as_diag_cof
  p offset_compensation as_diag_lf
  store (r ~> agc_value) (toRep (fromRep x #. as_diag_agc))
  where
    p lbl field = store (r ~> lbl) (bitToBool (fromRep x #. field))
