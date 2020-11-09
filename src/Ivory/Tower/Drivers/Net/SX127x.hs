{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BinaryLiterals #-}

module Ivory.Tower.Drivers.Net.SX127x (
    module Ivory.Tower.Drivers.Net.SX127x.Types
  , module Ivory.Tower.Drivers.Net.LoRa
  , sxTower
  ) where

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

import Ivory.Tower.Drivers.Net.LoRa
import Ivory.Tower.Drivers.Net.SX127x.Peripheral
import Ivory.Tower.Drivers.Net.SX127x.Regs
import Ivory.Tower.Drivers.Net.SX127x.RegTypes
import Ivory.Tower.Drivers.Net.SX127x.Types

-- | SX127x SPI FSK/LoRa radio driver
sxTower
  :: (IvoryArea init, IvoryZero init, Time t)
  => BackpressureTransmit ('Struct "spi_transaction_request")
                          ('Struct "spi_transaction_result")
  -> ChanOutput init -- ^ SPI ready
  -> SPIDeviceHandle
  -> String          -- ^ Instance name
  -> SXConfig t
  -> GPIOPin         -- ^ Reset pin
  -> Tower e ( ChanOutput ('Stored IBool)
             , BackpressureTransmit ('Struct "radio_request")
                                    ('Stored IBool)
             , BackpressureTransmit ('Struct "radio_listen")
                                    ('Struct "radio_result")
             )
sxTower (BackpressureTransmit req res) rdy spiDev name conf pin = do
  towerModule radioDriverTypes
  towerDepends radioDriverTypes

  maybePollingPeriod <- case sx_polling_period conf of
    Just pPer -> Just <$> period pPer
    Nothing -> pure Nothing
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

    linkConfig <- state (named "linkConfig")

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

    let trigger :: Emitter ('Struct "spi_transaction_request")
                -> Ivory eff ()
        trigger em = do
            ok <- deref isReady
            busy' <- deref busy
            when (ok .&& iNot busy') $ do
              store busy true
              sxRead (sxModemStatus sx127x) spiReq
              emit em (constRef spiReq)

    handler (snd radioTx) (named "transmitRequest") $ do
      reqE <- emitter req 1
      callback $ \r -> do
        ok <- deref isReady
        when ok $ do
          store isTX true
          refCopy txReq r
          trigger reqE

    handler (snd radioRx) (named "listenRequest") $ do
      reqE <- emitter req 1
      callback $ \r -> do
        ok <- deref isReady
        when ok $ do
          store isRX true
          refCopy rxReq r
          trigger reqE

    case maybePollingPeriod of
      Nothing -> return ()
      Just p -> do
        handler p (named "per") $ do
          reqE <- emitter req 1
          callback $ const $ do
            ok <- deref isReady
            busy' <- deref busy
            when (ok .&& iNot busy') $ do
              store busy true
              sxRead (sxModemStatus sx127x) spiReq
              emit reqE (constRef spiReq)

    case sx_isr conf of
      Nothing -> return ()
      Just isr ->
        handler isr (named "dioISR") $ do
          reqE <- emitter req 1
          callback $ const $ do
            trigger reqE

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
                store (arr ~> stringDataL ! 0) (bitCast $ frf `iShiftR` 16)
                store (arr ~> stringDataL ! 1) (bitCast $ frf `iShiftR` 8)
                store (arr ~> stringDataL ! 2) (bitCast $ frf)

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

            -- Compare value of previous radio_link struct field
            -- and only execute action when it's different from new value
            whenDiffers
               :: (IvoryStore a, IvoryEq a)
               => Ref 'Global ('Struct "radio_link")
               -> Label "radio_link" ('Stored a)
               -> (a -> Ivory eff ())
               -> Ivory eff ()
            whenDiffers rl label fun = do
              x <- deref (rl ~> label)
              x' <- deref (linkConfig ~> label)
              when (x /=? x') (fun x)

            configureLink rl = do
              whenDiffers rl radio_link_frequency $ \f -> do
                setFrequency f

              whenDiffers rl radio_link_iq_invert $ \doInvert -> do
                inv <- assign $ repToBits $ withBits 0x27 $ do
                  setField invert_iq_inverted (boolToBit doInvert)
                write $ sxWrite (sxInvertIQ sx127x) inv

              newBw <- deref (rl ~> radio_link_bandwidth)
              newCr <- deref (rl ~> radio_link_coding_rate)
              bw <- deref (linkConfig ~> radio_link_bandwidth)
              cr <- deref (linkConfig ~> radio_link_coding_rate)

              when (newBw /=? bw .|| newCr /=? cr) $ do
                bwField <- bandwidthToSx127x newBw
                crField <- codingRateToSx127x newCr
                modem1 <- assign $ repToBits $ withBits 0 $ do
                  setField modem_config1_bw bwField
                  setField modem_config1_cr crField
                write $ sxWrite (sxModemConfig1 sx127x) modem1

              whenDiffers rl radio_link_spreading $ \sf -> do
                sfField <- sfToSx127x sf
                modem2 <- assign $ repToBits $ withBits 0 $ do
                  setBit modem_config2_rx_payload_crc_on
                  setField modem_config2_spreading_factor sfField
                write $ sxWrite (sxModemConfig2 sx127x) modem2

              -- we need to set
              -- LowDataRateOptimize to 1
              -- mandated for when the symbol length exceeds 16ms
              sf <- deref (rl ~> radio_link_spreading)
              when ((symbolTime sf newBw) >? (fromIMilliseconds (16 :: Uint8))) $ do
                prev <- read $ sxRead (sxModemConfig3 sx127x)
                write
                  $ sxWrite (sxModemConfig3 sx127x)
                  $ repToBits $ withBits prev $ do
                    setBit modem_config3_low_data_rate_optimize

              -- store new linkConfig
              refCopy linkConfig rl

            intToBits :: Int -> Bits 8
            intToBits x = repToBits $ fromIntegral x

            bandwidthToSx127x x = cond [
                x ==? bw125 ==> return bandwidth_125_kHz
              , x ==? bw250 ==> return bandwidth_250_kHz
              , x ==? bw500 ==> return bandwidth_500_kHz
              ]

            codingRateToSx127x x = cond [
                x ==? cr1 ==> return cr_4over5bits
              , x ==? cr2 ==> return cr_4over6bits
              , x ==? cr3 ==> return cr_4over7bits
              , x ==? cr4 ==> return cr_4over8bits
              ]

            sfToSx127x x = cond [
                x ==? sf6  ==> return spreadingfactor_6
              , x ==? sf7  ==> return spreadingfactor_7
              , x ==? sf8  ==> return spreadingfactor_8
              , x ==? sf9  ==> return spreadingfactor_9
              , x ==? sf10 ==> return spreadingfactor_10
              , x ==? sf11 ==> return spreadingfactor_11
              , x ==? sf12 ==> return spreadingfactor_12
              ]

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

        case sx_lna conf of
          (LNAGainStatic gain boostHF) -> do
            lna <- assign $ repToBits $ withBits 0 $ do
              setField lna_gain_lna_gain (case gain of
                1 -> lna_gain_g1
                2 -> lna_gain_g2
                3 -> lna_gain_g3
                4 -> lna_gain_g4
                5 -> lna_gain_g5
                6 -> lna_gain_g6
                _ -> error "LNA Gain has to be in range <1-6> where 1 is highest gain"
                )
              setField lna_gain_lna_boost_hf (fromRep (case boostHF of
                False -> 0b00
                True  -> 0b11))

            write $ sxWrite (sxLNAGain sx127x) lna

          LNAGainAutomatic -> do
            modem3 <- assign $ repToBits $ withBits 0 $ do
              setBit modem_config3_agc_auto_on

            write $ sxWrite (sxModemConfig3 sx127x) modem3

        comment "Configure FIFOs"
        write $ sxWrite (sxFIFORxBaseAddr sx127x) (repToBits $ fromIntegral (0x0 :: Int))
        write $ sxWrite (sxFIFOTxBaseAddr sx127x) (repToBits $ fromIntegral (0x0 :: Int))

        comment "Max payload length"
        write $ sxWrite (sxMaxPayloadLength sx127x)
                 (repToBits $ fromIntegral (0x80 :: Int))

        comment "Power"
        -- for RFM95W only PA_BOOST is available
        case sx_output_pa_boost conf of
          True -> do
            comment $ "Using PA_BOOST, output power "
              ++ (show $ sx_output_power conf + 2) ++ "dBm"
            power <- assign $ repToBits $ withBits 0 $ do
              -- Pout = 2dB + output_power
              setBit pa_config_pa_select
              setField pa_config_output_power $
                fromRep $ fromIntegral $ sx_output_power conf
            write $ sxWrite (sxPAConfig sx127x) power
          False -> do
            comment $ "Using RFO, output power "
              ++ (show $ (10.8 :: Float) + 0.6 * (fromIntegral $ sx_output_max_power conf) - 15 + (fromIntegral $ sx_output_power conf)) ++ "dBm"

            power <- assign $ repToBits $ withBits 0 $ do
              setField pa_config_output_power $
                fromRep $ fromIntegral $ sx_output_power conf

              setField pa_config_max_power $
                fromRep $ fromIntegral $ sx_output_max_power conf
            write $ sxWrite (sxPAConfig sx127x) power

        -- DIO0 ISR on RX done and TX done
        dio0mapping <- assign $ repToBits $ withBits 0 $ do
          setField dio_mapping1_dio0 loraDIO0RxOrTxDone

        write $ sxWrite (sxDIOMapping1 sx127x) dio0mapping

        comment "Stand-by"
        switchMode mode_standby

        write $ sxWrite (sxSyncWord sx127x) (intToBits $ sx_sync_word conf)

        store isReady true
        emitV radioRdyE true

        forever $ noBreak $ do
          -- we get status from polling or interrupt handler above
          mStatus <- yield
          store busy false
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
                store (rxRes ~> radio_rx_buf ~> stringLengthL) (safeCast len)

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
                        store (rxRes ~> radio_rx_buf ~> stringDataL ! j) x
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

            configureLink (txReq ~> radio_tx_conf)

            write $ sxWrite (sxFIFOAddr sx127x) (intToBits 0x0)

            len <- fmap ((bitCast :: Uint32 -> Uint8) . signCast) $ deref (txReq ~> radio_tx_buf ~> stringLengthL)

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
            configureLink (rxReq ~> radio_listen_conf)

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
  -> ConstRef s2 SXBuffer
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
          x <- deref (dat ~> stringDataL ! i)
          store (r ~> tx_buf ! (toIx (fromIx i) + 1)) x
      ]

regAddr :: BitDataReg d -> Integer
regAddr r = case bdr_reg r of Reg a -> a
