{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Drivers.ADC.ADS1x1x
  ( adsTower
  , adsDefaultAddr
  , module Ivory.Tower.Drivers.ADC.ADS1x1x.Types
  ) where

import Control.Monad (forM_, void)
import Ivory.Language
import Ivory.Stdlib

import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.Bus.I2C
import Ivory.Tower.HAL.Bus.I2C.DeviceAddr

import Ivory.Tower.Drivers.ADC.ADS1x1x.Regs
import Ivory.Tower.Drivers.ADC.ADS1x1x.RegTypes
import Ivory.Tower.Drivers.ADC.ADS1x1x.Types

import qualified Data.List.NonEmpty

adsDefaultAddr :: I2CDeviceAddr
adsDefaultAddr = I2CDeviceAddr 0x48

-- | Driver for sampling ADS1x1x series chips.
--
-- Sending message to request channel triggers
-- conversion sequence specified via @ADSConfig@.
--
-- Driver always returns an Array of four elements
-- even if only one conversion is performed.
adsTower
  :: BackpressureTransmit
       (Struct "i2c_transaction_request")
       (Struct "i2c_transaction_result")
  -> ChanOutput (Stored ITime)
  -> I2CDeviceAddr
  -> ADSConfig
  -> Tower e
      (BackpressureTransmit
         (Stored ITime)
         (Array 4 (Stored IFloat))
      )
adsTower (BackpressureTransmit reqChan resChan) initChan addr adsConf@ADSConfig{..} = do
  readRequest <- channel
  readResponse <- channel

  monitor (named "Monitor") $ do
    adcLast <- state (named "Last")
    ready <- state (named "Ready")

    handler (snd readRequest) (named "ReadReq") $ do
      reqE <- emitter reqChan 1
      callback $ const $ do
        isReady <- deref ready
        when isReady $ do
          dummy <- readDevReg addr adsRegConfig
          emit reqE dummy

    coroutineHandler initChan resChan (named "Coro") $ do
      resE <- emitter (fst readResponse) 1
      reqE <- emitter reqChan 1
      return $ CoroutineBody $ \yield -> do

        let rpc req = do
              r <- req
              emit reqE r
              yield

            rpc_ = void . rpc

            merge :: Uint8 -> Uint8 -> Uint16
            merge lsb msb = (safeCast lsb) .| ((safeCast msb) `iShiftL` 8)

            -- | Read conversion register and convert to voltage
            readConversion = do
              v <- noBreak $ rpc $ readDevReg addr adsRegConversion
              hi' <- deref (v ~> rx_buf ! 0)
              lo' <- deref (v ~> rx_buf ! 1)

              assign
                $ (*(adsPGAToIFloat adsConfigPGA))
                $ (/2^(15 :: Int))
                $ safeCast
                $ twosComplementCast
                $ merge lo' hi'

        forever $ do
          cfg <- noBreak $ rpc $ readDevReg addr adsRegConfig
          rc <- cfg ~>* resultcode
          when (rc ==? 0) breakOut

        comment "Set configuration"
        rpc_
          $ writeDevReg
              addr
              adsRegConfig
              $ repToBits
              $ cfgReg adsConf

        store ready true

        forever $ do
          case adsConfigMode of
            ADSMode_Single muxSequence -> do
              forM_
                (zip
                  (map
                    adsMuxVal
                    (Data.List.NonEmpty.toList muxSequence)
                  )
                  [(0 :: Int)..3])
                $ \(mux, i) -> do

                cfgMux <- assign
                  $ repToBits
                  $ withBits (cfgReg adsConf)
                  $ do
                      setField config_os opPerformSingle
                      setField config_mux mux

                noBreak $ rpc_ $ writeDevReg addr adsRegConfig cfgMux

                -- loop until conversion is complete
                forever $ do
                  cfg' <- noBreak $ rpc $ readDevReg addr adsRegConfig

                  hi <- deref (cfg' ~> rx_buf ! 0)
                  lo <- deref (cfg' ~> rx_buf ! 1)
                  d <- assign $ (merge lo hi)

                  when
                    (fromRep d #. config_os ==? opStatusIdle)
                    breakOut

                  readConversion
                    >>= store (adcLast ! fromIntegral i)

            ADSMode_Continuous _mux -> do
              readConversion
              >>= store (adcLast ! 0)

          emit resE (constRef adcLast)

          comment "Wait for next read request"
          _ <- yield
          pure ()

  return $ BackpressureTransmit
            (fst readRequest)
            (snd readResponse)
  where
    named :: String -> String
    named nm = "ads" ++ nm

cfgReg :: ADSConfig -> BitDataRep CONFIG
cfgReg ADSConfig{..} = withBits 0 $ do
  setField config_mux mux
  setField config_pga $ adsPGAVal adsConfigPGA
  setField config_mode $ adsModeVal adsConfigMode
  setField config_datarate $ adsDataRateVal adsConfigDataRate
  setField config_comp_que cmpQueueDisable
  where
    mux =
        adsMuxVal
      $ case adsConfigMode of
          ADSMode_Continuous m -> m
          ADSMode_Single muxSeq -> Data.List.NonEmpty.head muxSeq

readDevReg :: (GetAlloc eff ~ 'Scope s)
           => I2CDeviceAddr
           -> AdsReg
           -> Ivory eff (ConstRef ('Stack s) ('Struct "i2c_transaction_request"))
readDevReg addr reg = fmap constRef $ local $ istruct
  [ tx_addr .= ival addr
  , tx_buf  .= iarray [ regVal ]
  , tx_len  .= ival 1
  , rx_len  .= ival 2
  ]
  where
    regVal :: Init ('Stored Uint8)
    regVal = ival $ toRep $ x
    x :: AdsReg
    x = fromRep $ withBits 0 $ setField adsReg reg

writeDevReg :: (GetAlloc eff ~ 'Scope s)
            => I2CDeviceAddr
            -> AdsReg
            -> Bits 16
            -> Ivory eff (ConstRef ('Stack s) ('Struct "i2c_transaction_request"))
writeDevReg addr regAddr dat = fmap constRef $ local $ istruct
  [ tx_addr .= ival addr
  , tx_buf  .= iarray [ regVal, h dat, l dat ]
  , tx_len  .= ival 3
  , rx_len  .= ival 0
  ]
  where
    regVal :: Init ('Stored Uint8)
    regVal = ival $ toRep $ reg
    reg :: AdsReg
    reg = fromRep $ withBits 0 $ setField adsReg regAddr
    l x = ival $ bitCast $ toRep x
    h x = ival $ bitCast $ (toRep x) `iShiftR` 8
