{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Ivory.Tower.Drivers.ADC.ADS1x1x where

import Control.Monad (forM_, void)
import Ivory.Language
import Ivory.Stdlib

import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.Bus.I2C
import Ivory.Tower.HAL.Bus.I2C.DeviceAddr

import Ivory.Tower.Drivers.ADC.ADS1x1x.Regs
import Ivory.Tower.Drivers.ADC.ADS1x1x.RegTypes

adsDefaultAddr :: I2CDeviceAddr
adsDefaultAddr = I2CDeviceAddr 0x48

named :: String -> String
named nm = "ads_" ++ nm

type ADCArray = 'Array 4 ('Stored Uint16)

defaults :: BitDataRep CONFIG
defaults = withBits 0 $ do
  setField config_mux muxDiff_0_1
  setField config_pga pga_fsr_2_048
  setField config_mode modeSingle
  setField config_datarate dr_1600
  setField config_comp_que cmpQueueDisable

-- on trigger read all 4 channels in single ended mode
adsTower :: BackpressureTransmit ('Struct "i2c_transaction_request") ('Struct "i2c_transaction_result")
         -> ChanOutput ('Stored ITime)
         -> I2CDeviceAddr
         -> Tower e (BackpressureTransmit
               ('Stored ITime)
               ADCArray)
adsTower (BackpressureTransmit reqChan resChan) initChan addr = do
  readRequest <- channel
  readResponse <- channel

  monitor (named "monitor") $ do
    adcLast <- state (named "last")

    dbg <- state (named "dbg")

    handler (snd readRequest) (named "readReq") $ do
      reqE <- emitter reqChan 1
      callback $ const $ do
        dummy <- readDevReg addr adsRegConfig
        emit reqE dummy

    coroutineHandler initChan resChan (named "coro") $ do
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

        forever $ do
          cfg <- noBreak $ rpc $ readDevReg addr adsRegConfig
          rc <- cfg ~>* resultcode
          when (rc ==? 0) breakOut

        cfg <- assign $ repToBits $ defaults
        rpc_ $ writeDevReg addr adsRegConfig cfg

        forever $ noBreak $ do
          forM_ (zip [ muxSingle_0
                     , muxSingle_1
                     , muxSingle_2
                     , muxSingle_3]
                     [(0 :: Int)..3]) $ \(mux, i) -> do

            cfgMux <- assign $ repToBits $ withBits defaults $ do
              setField config_os opStatusSingle
              setField config_mux mux

            -- just a check if we arent busy
            rpc_ $ writeDevReg addr adsRegConfig cfgMux
            cfg' <- rpc $ readDevReg addr adsRegConfig
            refCopy dbg cfg'

            hi <- deref (dbg ~> rx_buf ! 0)
            lo <- deref (dbg ~> rx_buf ! 1)
            d <- assign $ (merge lo hi)
            assert (fromRep d #. config_os ==? opStatusNoop)

            v <- rpc $ readDevReg addr adsRegConversion
            hi' <- deref (v ~> rx_buf ! 0)
            lo' <- deref (v ~> rx_buf ! 1)

            vx <- assign $ (merge lo' hi') `iShiftR` 3
            store (adcLast ! fromIntegral i) vx

          emit resE (constRef adcLast)

          _ <- yield
          return ()

  return $ BackpressureTransmit
            (fst readRequest)
            (snd readResponse)

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
