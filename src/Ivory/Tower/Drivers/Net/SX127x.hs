{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BinaryLiterals #-}
-- TYPES
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}


module Ivory.Tower.Drivers.Net.SX127x where

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

type SXArray = 'Array 128 ('Stored Uint8)

[ivory|
 struct radio_request
  { radio_tx_buf :: SXArray
  ; radio_tx_len :: Stored (Ix 128)
  }

 struct radio_listen
  { radio_listen_continuous :: Stored IBool
  ; radio_listen_frequency :: Stored Uint32
  }

 struct radio_result
  { radio_rx_buf :: SXArray
  ; radio_rx_len :: Stored (Ix 128)
  ; radio_rx_rssi :: Stored IFloat
  ; radio_rx_snr  :: Stored IFloat
  }
|]

radioDriverTypes :: Module
radioDriverTypes = package "radioDriverTypes" $ do
  defStruct (Proxy :: Proxy "radio_request")
  defStruct (Proxy :: Proxy "radio_listen")
  defStruct (Proxy :: Proxy "radio_result")

-- todo
-- ttn sync word 0x34

sxTower (BackpressureTransmit req res) rdy spiDev name pin receiver = do
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
    --dbg <- state (named "dbg")

    freq <- state (named "freq")

    rxCount <- stateInit (named "rxCount") (ival (0 :: Uint32))
    rxTimeout <- stateInit (named "rxTimeout") (ival (0 :: Uint32))
    rxCRCFail <- stateInit (named "rxCRCFail") (ival (0 :: Uint32))

    rxReq <- state (named "rxreq")
    rxRes <- state (named "rxres")

    txReq <- state (named "txreq")

    txAttempts <- stateInit (named "txAttempts") (ival (0 :: Uint32))
    txCount <- stateInit (named "txCount") (ival (0 :: Uint32))

    isReady <- state (named "isReady")
    isTX <- state (named "isTX")
    inTX <- state (named "inTX")
    isRX <- state (named "isRX")
    inRX <- state (named "inRX")

    resetDevice <- state (named "resetDevice")
    resetDone   <- state (named "resetDone")
    resLow      <- state (named "resLow")

    handler systemInit (named "pinSet") $ do
      callback $ const $ do
        pinEnable   pin
        pinSetMode  pin gpio_mode_input
        pinSetPUPD  pin gpio_pupd_none
        pinSetSpeed pin gpio_speed_50mhz

    handler resetPeriod (named "resetPeriod") $ do
      reqE <- emitter req 1
      callback $ const $ do
        res <- deref resetDevice
        when res $ do

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

              r <- sxRead spiDev (sxModemStatus sx127x)
              emit reqE r
            )

    handler (snd radioTx) (named "transmitPeriod") $ do
      reqE <- emitter req 1
      callback $ \r -> do
        ok <- deref isReady
        when ok $ do
          store isTX true
          refCopy txReq r

    handler (snd radioRx) (named "listenRequest") $ do
      reqE <- emitter req 1
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
        when ok $ do
          r <- sxRead spiDev (sxModemStatus sx127x)
          emit reqE r

    coroutineHandler rdy res "sxCoro" $ do
      reqE <- emitter req 1
      radioRdyE <- emitter (fst radioRdy) 1
      radioRxDoneE <- emitter (fst radioRxDone) 1
      radioTxDoneE <- emitter (fst radioTxDone) 1
      return $ CoroutineBody $ \yield -> do
        let rpc x = do
              r <- x
              emit reqE r
              yield

            read x = do
              res <- rpc x
              deref (res ~> rx_buf ! 1)

            write x = rpc x >>= const (return ())
            setFrequency freq = do
                frf <- assign $ (((safeCast :: Uint32 -> Uint64) freq) `iShiftL` 19) `iDiv` 32_000_000
                arr <- local $ izero
                store (arr ! 0) (bitCast $ frf `iShiftR` 16)
                store (arr ! 1) (bitCast $ frf `iShiftR` 8)
                store (arr ! 2) (bitCast $ frf)

                write $ sxWriteArray spiDev (sxFrfMsb sx127x) (constRef arr) 3

            getFrequency = do
                dat <- rpc $ sxReadMany spiDev (sxFrfMsb sx127x) 3
                hi  <- fmap safeCast $ deref (dat ~> rx_buf ! 1)
                mid <- fmap safeCast $ deref (dat ~> rx_buf ! 2)
                low <- fmap safeCast $ deref (dat ~> rx_buf ! 3)
                frf <- assign $ hi `iShiftL` 16 + mid `iShiftL` 8 + (low :: Uint64)
                store freq $ frf * 32_000_000 `iShiftR` 19

            switchMode opmode = do
                prevRes <- rpc $ sxRead spiDev (sxMode sx127x)
                prev <- deref (prevRes ~> rx_buf ! 1)
                mode <- assign $ repToBits $ withBits prev $ do
                  setField op_mode_mode opmode

                write $ sxWrite spiDev (sxMode sx127x) mode
                comment "Make sure we've switched mode"
                forever $ do
                  x <- noBreak $ rpc $ sxRead spiDev (sxMode sx127x)
                  vers <- fmap fromRep $ deref (x ~> rx_buf ! 1)
                  when (vers #. op_mode_mode ==? opmode) breakOut

            intToBits :: Int -> Bits 8
            intToBits x = repToBits $ fromIntegral x

        store resetDevice true
        -- wait untill timing sensitive device reset is done
        _ <- yield

        forever $ do
          vers <- noBreak $ read $ sxRead spiDev (sxVersion sx127x)
          when (vers ==? 0x12) breakOut

        comment "Put radio to sleep"
        switchMode mode_sleep

        comment "Enable LORA"
        lora <- assign $ repToBits $ withBits 0 $ do
          setBit op_mode_long_range_mode
          -- for HF
          clearBit op_mode_low_frequency_mode_on

        write $ sxWrite spiDev (sxMode sx127x) lora

        --setFrequency 868_000_000
        setFrequency 868_100_000
        --setFrequency 434_000_000
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

        write $ sxWrite spiDev (sxModemConfig1 sx127x) modem1
        write $ sxWrite spiDev (sxModemConfig2 sx127x) modem2
        --write $ sxWrite spiDev (sxModemConfig3 sx127x) modem3

        comment "Configure FIFOs"
        write $ sxWrite spiDev (sxFIFORxBaseAddr sx127x) (repToBits $ fromIntegral (0x0 :: Int))
        write $ sxWrite spiDev (sxFIFOTxBaseAddr sx127x) (repToBits $ fromIntegral (0x0 :: Int))

        comment "Max payload length"
        write $ sxWrite spiDev (sxMaxPayloadLength sx127x)
                 (repToBits $ fromIntegral (0x80 :: Int))

        comment "Power"
        -- for RFM95W only PA_BOOST is available
        power <- assign $ repToBits $ withBits 0 $ do
          setBit pa_config_pa_select -- use PA_BOOST
          -- when PA_BOOST
          -- Pout = 2dB + output_power
          setField pa_config_output_power (fromRep 0x0)
        write $ sxWrite spiDev (sxPAConfig sx127x) power

        -- XXX
        unless receiver $ do
          write $ sxWrite spiDev (sxDIOMapping1 sx127x) (repToBits $ fromIntegral (0x40 :: Int))

        comment "Stand-by"
        switchMode mode_standby

        -- XXX - WAN
        write $ sxWrite spiDev (sxSyncWord sx127x) (intToBits 0x34)

        store isReady true
        emitV radioRdyE true

        forever $ noBreak $ do
          -- we get status from polling handler above
          -- XXX: switch to exti
          mStatus <- yield

          --x <- rpc $ sxRead spiDev (sxMode sx127x)
          --refCopy dbg x

          i <- rpc $ sxRead spiDev (sxIRQFlags sx127x)
          --refCopy dbgisr i

          is <- deref (i ~> rx_buf ! 1)
          isrs <- assign $ fromRep is
          cond_ [
              -- rx timeout only fires in RX Single mode
              bitToBool (isrs #. irq_flags_rx_timeout) ==> rxTimeout += 1
            , bitToBool (isrs #. irq_flags_payload_crc_error) ==> rxCRCFail += 1
            , bitToBool (isrs #. irq_flags_tx_done) ==> do
                txCount += 1
                emitV radioTxDoneE true

            , bitToBool (isrs #. irq_flags_rx_done) ==> do
                rxCount += 1
                len <- read $ sxRead spiDev (sxRxLength sx127x)
                store (rxRes ~> radio_rx_len) (toIx len)

                -- RSSI[dBm] = -164 + Rssi (using LF output port, SNR >= 0)
                -- RSSI[dBm] = -157 + Rssi (using HF output port, SNR >= 0)
                i <- read $ sxRead spiDev (sxPacketRSSI sx127x)
                s <- read $ sxRead spiDev (sxPacketSNR sx127x)
                store (rxRes ~> radio_rx_rssi) (safeCast (i - 157))
                store (rxRes ~> radio_rx_snr) (safeCast (twosComplementCast s `iDiv` 4))

                -- set FIFO to RxCurrentAddr
                rxAddr <- read $ sxRead spiDev (sxFIFORxCurrentAddr sx127x)

                write $ sxWrite spiDev (sxFIFOAddr sx127x) (fromRep rxAddr)
                dat <- rpc $ sxReadMany spiDev (sxFIFO sx127x) len

                -- Read all received bytes
                w <-  sxReadMany spiDev (sxFIFO sx127x) len
                -- we need to skip rx_buf ! 0
                arrayMap $ \i -> do
                  cond_ [
                      fromIx i >=? safeCast len ==> return ()
                    , true      ==> do
                        x <- deref (dat ~> rx_buf ! (toIx (fromIx i) + 1))
                        store (rxRes ~> radio_rx_buf ! i) x
                    ]

                emit radioRxDoneE (constRef rxRes)
                emitV radioRdyE true

            ]

          comment "Clear IRQ flags"
          when (is /=? 0) $ do
            write $ sxWrite spiDev (sxIRQFlags sx127x) (repToBits is)

          tx <- deref isTX
          when tx $ do

            switchMode mode_standby
            inv <- assign $ repToBits $ withBits 0x27 $ do
              clearBit invert_iq_inverted
            write $ sxWrite spiDev (sxInvertIQ sx127x) inv

            write $ sxWrite spiDev (sxFIFOAddr sx127x) (intToBits 0x0)

            len <- fmap ixToU8 $ deref (txReq ~> radio_tx_len)

            write $ sxWrite spiDev (sxPayloadLength sx127x)
              (repToBits $ len)

            write $ sxWriteArray spiDev (sxFIFO sx127x)
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

            write $ sxWrite spiDev (sxLNAGain sx127x) lna

            -- 0x27 but why
            inv <- assign $ repToBits $ withBits 0x27 $ do
              setBit invert_iq_inverted

            write $ sxWrite spiDev (sxInvertIQ sx127x) inv

            modeCont <- deref (rxReq ~> radio_listen_continuous)
            switchMode (modeCont ? (mode_rxcontinuous, mode_rxsingle))

            store inRX true
            store isRX false

        return ()

  return ( snd radioRdy
         , BackpressureTransmit (fst radioTx) (snd radioTxDone)
         , BackpressureTransmit (fst radioRx) (snd radioRxDone)
         )
  where
    named x = intercalate "_" [ "sx127x", name, x ]

sxRead  :: (GetAlloc eff ~ 'Scope s)
        => SPIDeviceHandle
        -> BitDataReg a
        -> Ivory eff (ConstRef ('Stack s) ('Struct "spi_transaction_request"))
sxRead dev bdr = fmap constRef $ local $ istruct
  [ tx_device .= ival dev
  , tx_buf .= iarray ( map ival [fromIntegral $ regAddr bdr, 0] )
  , tx_len .= ival 2 ]

sxWrite  :: (GetAlloc eff ~ 'Scope s)
         => SPIDeviceHandle
         -> BitDataReg a
         -> Bits 8
         -> Ivory eff (ConstRef ('Stack s) ('Struct "spi_transaction_request"))
sxWrite dev bdr dat = fmap constRef $ local $ istruct
  [ tx_device .= ival dev
  , tx_buf .= iarray (map ival
    [ 0x80 .| (fromIntegral $ regAddr bdr)
    , toRep dat]
  )
  , tx_len .= ival 2 ]

sxWriteMany  :: (GetAlloc eff ~ 'Scope s)
         => SPIDeviceHandle
         -> BitDataReg a
         -> [Int]
         -> Ivory eff (ConstRef ('Stack s) ('Struct "spi_transaction_request"))
sxWriteMany dev bdr dat = fmap constRef $ local $ istruct
  [ tx_device .= ival dev
  , tx_buf .= iarray (map ival
    [ 0x80 .| (fromIntegral $ regAddr bdr)]
    ++ (map (ival . fromIntegral) dat)
  )
  , tx_len .= ival (fromIntegral $ length dat + 1) ]

sxWriteArray  :: (GetAlloc eff ~ 'Scope s)
         => SPIDeviceHandle
         -> BitDataReg a
         -> ConstRef s2 SXArray
         -> Uint8
         -> Ivory eff (ConstRef ('Stack s) ('Struct "spi_transaction_request"))
sxWriteArray dev bdr dat len = do
  header <- local $ istruct
    [ tx_device .= ival dev
    , tx_buf .= iarray [ ival $ 0x80 .| (fromIntegral $ regAddr bdr) ]
    , tx_len .= ival (toIx $ len + 1) ]

  arrayMap $ \i -> do
    cond_ [
        fromIx i >=? safeCast len ==> return ()
      , true      ==> do
          x <- deref (dat ! i)
          store (header ~> tx_buf ! (toIx (fromIx i) + 1)) x
      ]

  return $ constRef $ header

sxReadMany  :: (GetAlloc eff ~ 'Scope s)
         => SPIDeviceHandle
         -> BitDataReg a
         -> Uint8
         -> Ivory eff (ConstRef ('Stack s) ('Struct "spi_transaction_request"))
sxReadMany dev bdr len = fmap constRef $ local $ istruct
  [ tx_device .= ival dev
  , tx_buf .= iarray (map ival
    [ fromIntegral $ regAddr bdr]
  )
  , tx_len .= ival (toIx $ len + 1) ]

regAddr :: BitDataReg d -> Integer
regAddr r = case bdr_reg r of Reg a -> a
