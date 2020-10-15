{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BinaryLiterals #-}

module Ivory.Tower.Drivers.Net.SX127x where

import Prelude hiding (read)
import Data.List (intercalate)

import Ivory.Language
import Ivory.Stdlib
import Ivory.HW
import Ivory.HW.BitData
import Ivory.HW.Reg
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.Bus.SPI
import Ivory.Tower.HAL.Bus.SPI.DeviceHandle

import Ivory.BSP.STM32.Peripheral.GPIO

import Ivory.Base (ixToU8)

import Ivory.Tower.Drivers.Net.SX127x.Peripheral
import Ivory.Tower.Drivers.Net.SX127x.Regs
import Ivory.Tower.Drivers.Net.SX127x.RegTypes
import Ivory.Tower.Drivers.Net.SX127x.Types

-- todo
-- ttn sync word 0x34

sxTower (BackpressureTransmit req res) rdy spiDev name pin = do
  towerModule radioDriverTypes
  towerDepends radioDriverTypes

  p <- period (Milliseconds 100)
  resetPeriod <- period (Milliseconds 100)

  radioRdy    <- channel
  radioTx     <- channel
  radioTxDone <- channel
  radioRx     <- channel
  radioRxDone <- channel

  monitor "rfm" $ do
    dbgIRQFlags <- state (named "dbgIRQFlags")
    dbgModemStatus <- state (named "dbgModemStatus")
    busy <- state (named "busy")

    freq <- state (named "freq")

    rxCount <- stateInit (named "rxCount") (ival (0 :: Uint32))
    rxTimeout <- stateInit (named "rxTimeout") (ival (0 :: Uint32))
    rxCRCFail <- stateInit (named "rxCRCFail") (ival (0 :: Uint32))

    rxReq <- state (named "rxreq")
    rxRes <- state (named "rxres")

    txReq <- state (named "txreq")

    txAttempts <- stateInit (named "txAttempts") (ival (0 :: Uint32))
    rxAttempts <- stateInit (named "rxAttempts") (ival (0 :: Uint32))
    txCount <- stateInit (named "txCount") (ival (0 :: Uint32))

    isReady <- state (named "isReady")
    isTX <- state (named "isTX")
    inTX <- state (named "inTX")
    isRX <- state (named "isRX")
    inRX <- state (named "inRX")

    resetDevice <- state (named "resetDevice")
    resetDone   <- state (named "resetDone")
    resLow      <- state (named "resLow")

    spiReq <- stateInit (named "spiReq") (istruct [ tx_device .= ival spiDev ])

    handler systemInit (named "pinSet") $ do
      callback $ const $ do
        pinEnable   pin
        pinSetMode  pin gpio_mode_input
        pinSetPUPD  pin gpio_pupd_none
        pinSetSpeed pin gpio_speed_50mhz

    handler resetPeriod (named "resetPeriod") $ do
      reqE <- emitter req 1
      callback $ const $ do
        doReset <- deref resetDevice
        when doReset $ do

          rl <- deref resLow

          ifte_ (iNot rl)
            (do
              pinSetMode pin gpio_mode_output
              pinClear pin
              store resLow true
            )
            (do
              pinSetMode pin gpio_mode_input
              store resLow false
              store resetDevice false
              store resetDone true

              sxRead (sxModemStatus sx127x) spiReq
              emit reqE (constRef spiReq)
            )

    handler (snd radioTx) (named "transmitRequest") $ do
      callback $ \r -> do
        ok <- deref isReady
        when ok $ do
          store isTX true
          refCopy txReq r

    handler (snd radioRx) (named "listenRequest") $ do
      callback $ \r -> do
        ok <- deref isReady
        when ok $ do
          store isRX true
          refCopy rxReq r

    -- XXX: polling
    handler p (named "per") $ do
      reqE <- emitter req 1
      callback $ const $ do
        ok <- deref isReady
        busy' <- deref busy
        when (ok .&& iNot busy') $ do
          sxRead (sxModemStatus sx127x) spiReq
          emit reqE (constRef spiReq)

    coroutineHandler rdy res "sxCoro" $ do
      reqE <- emitter req 1
      radioRdyE <- emitter (fst radioRdy) 1
      radioRxDoneE <- emitter (fst radioRxDone) 1
      radioTxDoneE <- emitter (fst radioTxDone) 1
      return $ CoroutineBody $ \yield -> do

        let rpc createReq = do
              createReq spiReq
              store busy true
              emit reqE (constRef spiReq)
              x <- yield
              store busy false
              return x

            read x = do
              result <- rpc x
              deref (result ~> rx_buf ! 1)

            write x = rpc x >>= const (return ())
            setFrequency newFreq = do
                frf <- assign $ (((safeCast :: Uint32 -> Uint64) newFreq) `iShiftL` 19) `iDiv` 32_000_000
                arr <- local $ izero
                store (arr ! 0) (bitCast $ frf `iShiftR` 16)
                store (arr ! 1) (bitCast $ frf `iShiftR` 8)
                store (arr ! 2) (bitCast $ frf)

                write $ sxWriteArray (sxFrfMsb sx127x) (constRef arr) 3
                getFrequency

            getFrequency = do
                dat <- rpc $ sxReadN (sxFrfMsb sx127x) 3
                hi  <- fmap safeCast $ deref (dat ~> rx_buf ! 1)
                mid <- fmap safeCast $ deref (dat ~> rx_buf ! 2)
                low <- fmap safeCast $ deref (dat ~> rx_buf ! 3)
                frf <- assign $ hi `iShiftL` 16 + mid `iShiftL` 8 + (low :: Uint64)
                store freq $ frf * 32_000_000 `iShiftR` 19

            switchMode opmode = do
                prev <- read $ sxRead (sxMode sx127x)
                mode <- assign $ repToBits $ withBits prev $ do
                  setField op_mode_mode opmode

                write $ sxWrite (sxMode sx127x) mode
                comment "Make sure we've switched mode"
                forever $ do
                  x <- noBreak $ read $ sxRead (sxMode sx127x)
                  let mode' = fromRep x
                  when (mode' #. op_mode_mode ==? opmode) breakOut
                  noBreak $ write $ sxWrite (sxMode sx127x) mode

            intToBits :: Int -> Bits 8
            intToBits x = repToBits $ fromIntegral x

        store resetDevice true
        -- wait untill timing sensitive device reset is done
        _ <- yield

        forever $ do
          vers <- noBreak $ read $ sxRead (sxVersion sx127x)
          when (vers ==? 0x12) breakOut

        comment "Put radio to sleep"
        switchMode mode_sleep

        comment "Enable LORA"
        lora <- assign $ repToBits $ withBits 0 $ do
          setBit op_mode_long_range_mode
          -- for HF
          clearBit op_mode_low_frequency_mode_on

        write $ sxWrite (sxMode sx127x) lora

        getFrequency

        comment "Configure modem"
        modem1 <- assign $ repToBits $ withBits 0 $ do
          setField modem_config1_bw bandwidth_125_kHz
          setField modem_config1_cr cr_4over5bits

        modem2 <- assign $ repToBits $ withBits 0 $ do
          setBit modem_config2_rx_payload_crc_on
          --- XXX: hardcoded
          setField modem_config2_spreading_factor spreadingfactor_9
          --setField modem_config2_spreading_factor spreadingfactor_7

        --modem3 <- assign $ repToBits $ withBits 0 $ do
        --  setBit modem_config3_agc_auto_on

        write $ sxWrite (sxModemConfig1 sx127x) modem1
        write $ sxWrite (sxModemConfig2 sx127x) modem2
        --write $ sxWrite spiDev (sxModemConfig3 sx127x) modem3

        comment "Configure FIFOs"
        write $ sxWrite (sxFIFORxBaseAddr sx127x) (repToBits $ fromIntegral (0x0 :: Int))
        write $ sxWrite (sxFIFOTxBaseAddr sx127x) (repToBits $ fromIntegral (0x0 :: Int))

        comment "Max payload length"
        write $ sxWrite (sxMaxPayloadLength sx127x)
                 (repToBits $ fromIntegral (0x80 :: Int))

        comment "Power"
        -- for RFM95W only PA_BOOST is available
        power <- assign $ repToBits $ withBits 0 $ do
          setBit pa_config_pa_select -- use PA_BOOST
          -- when PA_BOOST
          -- Pout = 2dB + output_power
          setField pa_config_output_power (fromRep 0x0)
        write $ sxWrite (sxPAConfig sx127x) power

        -- DIO0 ISR on rxdone and txdone
        write $ sxWrite (sxDIOMapping1 sx127x) (repToBits $ fromIntegral (0b11 :: Int))

        comment "Stand-by"
        switchMode mode_standby

        store isReady true
        emitV radioRdyE true

        forever $ noBreak $ do
          -- we get status from polling handler above
          -- XXX: switch to exti
          mStatus <- yield
          refCopy dbgModemStatus mStatus

          i <- rpc $ sxRead (sxIRQFlags sx127x)
          refCopy dbgIRQFlags i

          is <- deref (i ~> rx_buf ! 1)
          isrs <- assign $ fromRep is
          cond_ [
              -- rx timeout only fires in RX Single mode
              bitToBool (isrs #. irq_flags_rx_timeout) ==> do
                rxTimeout += 1
                store inRX false
            , bitToBool (isrs #. irq_flags_payload_crc_error) ==> do
                rxCRCFail += 1
                store inRX false
            , bitToBool (isrs #. irq_flags_tx_done) ==> do
                txCount += 1
                emitV radioTxDoneE true

            , bitToBool (isrs #. irq_flags_rx_done) ==> do
                rxCount += 1
                len <- read $ sxRead (sxRxLength sx127x)
                store (rxRes ~> radio_rx_len) (toIx len)

                -- RSSI[dBm] = -164 + Rssi (using LF output port, SNR >= 0)
                -- RSSI[dBm] = -157 + Rssi (using HF output port, SNR >= 0)
                r <- read $ sxRead (sxPacketRSSI sx127x)
                s <- read $ sxRead (sxPacketSNR sx127x)
                store (rxRes ~> radio_rx_rssi) (safeCast r - 157)
                store (rxRes ~> radio_rx_snr) (safeCast (twosComplementCast s) / 4)

                -- set FIFO to RxCurrentAddr
                rxAddr <- read $ sxRead (sxFIFORxCurrentAddr sx127x)

                write $ sxWrite (sxFIFOAddr sx127x) (fromRep rxAddr)
                -- Read all received bytes
                dat <- rpc $ sxReadN (sxFIFO sx127x) len

                -- we need to skip rx_buf ! 0
                arrayMap $ \j -> do
                  cond_ [
                      fromIx j >=? safeCast len ==> return ()
                    , true      ==> do
                        x <- deref (dat ~> rx_buf ! (toIx (fromIx j) + 1))
                        store (rxRes ~> radio_rx_buf ! j) x
                    ]

                emit radioRxDoneE (constRef rxRes)
                emitV radioRdyE true

            ]

          comment "Clear IRQ flags"
          when (is /=? 0) $ do
            write $ sxWrite (sxIRQFlags sx127x) (repToBits is)

          tx <- deref isTX
          when tx $ do
            switchMode mode_standby

            f <- deref (txReq ~> radio_tx_frequency)
            setFrequency f

            write $ sxWrite (sxFIFOAddr sx127x) (intToBits 0x0)

            len <- fmap ixToU8 $ deref (txReq ~> radio_tx_len)

            write $ sxWrite (sxPayloadLength sx127x)
              (repToBits $ len)

            write $ sxWriteArray (sxFIFO sx127x)
              (constRef $ txReq ~> radio_tx_buf) len

            switchMode mode_tx
            store isTX false
            store inTX true
            txAttempts += 1

          rx <- deref isRX
          when rx $ do
            switchMode mode_standby
            f <- deref (rxReq ~> radio_listen_frequency)
            setFrequency f

            lna <- assign $ repToBits $ withBits 0 $ do
              setField lna_gain_lna_gain lna_gain_g1
              setField lna_gain_lna_boost_hf (fromRep 0b11)

            write $ sxWrite (sxLNAGain sx127x) lna

            modeCont <- deref (rxReq ~> radio_listen_continuous)
            switchMode (modeCont ? (mode_rxcontinuous, mode_rxsingle))

            store inRX true
            store isRX false
            rxAttempts += 1

        return ()

  return ( snd radioRdy
         , BackpressureTransmit (fst radioTx) (snd radioTxDone)
         , BackpressureTransmit (fst radioRx) (snd radioRxDone)
         )
  where
    named x = intercalate "_" [ "sx127x", name, x ]

sxReadN
  :: BitDataReg a
  -> Uint8
  -> Ref s ('Struct "spi_transaction_request")
  -> Ivory eff ()
sxReadN bdr len r = do
  store (r ~> tx_buf ! 0) (fromIntegral $ regAddr bdr)
  store (r ~> tx_buf ! 1) 0
  store (r ~> tx_len) (toIx $ len + 1)

sxRead
  :: BitDataReg a
  -> Ref s ('Struct "spi_transaction_request")
  -> Ivory eff ()
sxRead bdr = sxReadN bdr 1

sxWrite
  :: BitDataReg a
  -> Bits 8
  -> Ref s ('Struct "spi_transaction_request")
  -> Ivory eff ()
sxWrite bdr dat r = do
  store (r ~> tx_buf ! 0) (0x80 .| (fromIntegral $ regAddr bdr))
  store (r ~> tx_buf ! 1) (toRep dat)
  store (r ~> tx_len) 2

sxWriteArray
  :: BitDataReg a
  -> ConstRef s2 SXArray
  -> Uint8
  -> Ref s ('Struct "spi_transaction_request")
  -> Ivory eff ()
sxWriteArray bdr dat len r = do
  store (r ~> tx_buf ! 0) (0x80 .| (fromIntegral $ regAddr bdr))
  store (r ~> tx_len) (toIx $ len + 1)

  arrayMap $ \i -> do
    cond_ [
        fromIx i >=? safeCast len ==> return ()
      , true      ==> do
          x <- deref (dat ! i)
          store (r ~> tx_buf ! (toIx (fromIx i) + 1)) x
      ]

regAddr :: BitDataReg d -> Integer
regAddr r = case bdr_reg r of Reg a -> a
