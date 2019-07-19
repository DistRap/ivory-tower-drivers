{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.Tower.Drivers.Temperature.HTS221 where

import Ivory.Language
import Ivory.Stdlib
import Ivory.HW
import Ivory.HW.BitData
import Ivory.HW.Reg

import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.Bus.I2C
import Ivory.Tower.HAL.Bus.I2C.DeviceAddr

import Ivory.Base.Math (linearlyInterpolate)

import Ivory.Tower.Drivers.Temperature.Types
import Ivory.Tower.Drivers.Temperature.HTS221.Regs
import Ivory.Tower.Drivers.Temperature.HTS221.RegTypes
import Ivory.Tower.Drivers.Temperature.HTS221.Peripheral

named :: String -> String
named nm = "hts221_" ++ nm

hts221TypesModule :: Module
hts221TypesModule = package "hts221_types" $ do
  defStruct (Proxy :: Proxy "sample_th")

hts221Tower :: BackpressureTransmit ('Struct "i2c_transaction_request") ('Struct "i2c_transaction_result")
            -> ChanOutput ('Stored ITime)
            -> I2CDeviceAddr
            -> Tower e (ChanOutput ('Struct "sample_th"))
hts221Tower (BackpressureTransmit reqChan resChan) initChan addr = do
  towerModule hts221TypesModule
  towerDepends hts221TypesModule

  (sampleIn, sampleOut) <- channel
  per <- period (Milliseconds 1000)

  monitor (named "monitor") $ do
    (tt0 :: Ref 'Global ('Stored Sint16)) <- state "t0"
    (tt1 :: Ref 'Global ('Stored Sint16)) <- state "t1"
    (td0 :: Ref 'Global ('Stored Uint16)) <- state "t0d"
    (td1 :: Ref 'Global ('Stored Uint16)) <- state "t1d"

    (rawt :: Ref 'Global ('Stored Sint16)) <- state "rawt"
    (rawh :: Ref 'Global ('Stored Sint16)) <- state "rawh"

    coroutineHandler initChan resChan (named "coroutine") $ do
      reqE <- emitter reqChan 1
      sampleE <- emitter sampleIn 1
      return $ CoroutineBody $ \yield -> do

        let rpc req = req >>= emit reqE >> yield
            read req = do
              res <- rpc req
              code <- deref (res ~> resultcode)
              assert (code ==? 0)
              deref ((res ~> rx_buf) ! 0)

            readSigned16 req = do
              res <- rpc req
              code <- deref (res ~> resultcode)
              assert (code ==? 0)
              lsb <- deref ((res ~> rx_buf) ! 0)
              msb <- deref ((res ~> rx_buf) ! 1)
              return $ (twosComplementCast :: Uint16 -> Sint16)
                     $ ((safeCast :: Uint8 -> Uint16) lsb) .| ((safeCast msb) `iShiftL` 8)

        forever $ do
          whoami <- noBreak $ rpc $ readDevReg addr (htsWhoAmI hts221)
          -- XXX: check resultcode, limit retries, send ready
          actual <- deref ((whoami ~> rx_buf) ! 0)
          when (actual ==? 0xBC) $ breakOut

        comment "perform setup"
        ctrl1Data <- assign $ repToBits $ withBits 0 $ do
          setField power powerUp
          setField blockDataUpdate bduBlock
          setField outputDataRate rate1Hz

        ctrl2Data <- assign $ repToBits $ withBits 0 $ do
          setBit rebootMemory

        res <- rpc $ writeDevReg addr (htsCtrl1 hts221) ctrl1Data
        code <- deref (res ~> resultcode)
        assert (code ==? 0)

        res <- rpc $ writeDevReg addr (htsCtrl2 hts221) ctrl2Data
        code <- deref (res ~> resultcode)
        assert (code ==? 0)

        h0Rh <- read $ readDevReg addr (htsCalH0_RH_x2 hts221)
        h1Rh <- read $ readDevReg addr (htsCalH1_RH_x2 hts221)

        t0Deg <- read $ readDevReg addr (htsCalT0_DegC_x8 hts221)
        t1Deg <- read $ readDevReg addr (htsCalT1_DegC_x8 hts221)
        t1t0Msb <- read $ readDevReg addr (htsCalT1T0Msb hts221)

        h0t0 <- readSigned16 $ readSignedDevReg addr (htsCalH0T0OutLow hts221)
        h1t0 <- readSigned16 $ readSignedDevReg addr (htsCalH1T0OutLow hts221)

        t0 <- readSigned16 $ readSignedDevReg addr (htsCalT0OutLow hts221)
        t1 <- readSigned16 $ readSignedDevReg addr (htsCalT1OutLow hts221)

        let merge :: Uint8 -> Uint8 -> Uint16
            merge lsb msb = (safeCast lsb) .| ((safeCast msb) `iShiftL` 8)

        -- last 4 bits of t1t0Msb - t1.9 t1.8 t0.9 t0.8
        t0d  <- assign $ merge t0Deg (t1t0Msb .& 0x3)
        t1d  <- assign $ merge t1Deg ((t1t0Msb `iShiftR` 2) .& 0x3)

        store tt0 t0
        store tt1 t1
        store td0 t0d
        store td1 t1d

        forever $ noBreak $ do
          _ <- yield

          tempRaw <- readSigned16 $ readSignedDevReg addr (htsTempLow hts221)
          temp <- assign $ linearlyInterpolate (safeCast t0, (safeCast t0d) / 8)
                                               (safeCast t1, (safeCast t1d) / 8)
                                               (safeCast tempRaw)
          store rawt tempRaw

          humiRaw <- readSigned16 $ readSignedDevReg addr (htsHumiLow hts221)
          humi <- assign $ linearlyInterpolate (safeCast h0t0, (safeCast h0Rh) / 2)
                                               (safeCast h1t0, (safeCast h1Rh) / 2)
                                               (safeCast humiRaw)
          store rawh humiRaw

          t <- getTime

          res <- local $ istruct [
              sample_th_temperature .= ival temp
            , sample_th_humidity    .= ival humi
            , sample_th_time        .= ival t
            ]

          emit sampleE (constRef res)

    handler per (named "periodic_readout") $ do
      reqE <- emitter reqChan 1
      callback $ const $ do
        dummy <- readDevReg addr (htsWhoAmI hts221)
        emit reqE dummy

  return sampleOut

readDevReg :: (GetAlloc eff ~ 'Scope s)
           => I2CDeviceAddr
           -> BitDataReg a
           -> Ivory eff (ConstRef ('Stack s) ('Struct "i2c_transaction_request"))
readDevReg addr bdr = fmap constRef $ local $ istruct
  [ tx_addr .= ival addr
  , tx_buf  .= iarray [ival (fromIntegral $ regAddr bdr)]
  , tx_len  .= ival 1
  , rx_len  .= ival 1
  ]

readSignedDevReg :: (GetAlloc eff ~ 'Scope s)
           => I2CDeviceAddr
           -> BitDataReg a
           -> Ivory eff (ConstRef ('Stack s) ('Struct "i2c_transaction_request"))
readSignedDevReg addr bdr = fmap constRef $ local $ istruct
  [ tx_addr .= ival addr
  , tx_buf  .= iarray [ival (i2cAutoincrement .| (fromIntegral $ regAddr bdr))]
  , tx_len  .= ival 1
  , rx_len  .= ival 2
  ]
  where i2cAutoincrement = 0x80

writeDevReg :: (GetAlloc eff ~ 'Scope s)
            => I2CDeviceAddr
            -> BitDataReg a
            -> Bits 8
            -> Ivory eff (ConstRef ('Stack s) ('Struct "i2c_transaction_request"))
writeDevReg addr bdr dat = fmap constRef $ local $ istruct
  [ tx_addr .= ival addr
  , tx_buf  .= iarray
    [ ival (fromIntegral $ regAddr bdr)
    , ival $ toRep $ dat
    ]
  , tx_len  .= ival 2
  , rx_len  .= ival 0
  ]

regAddr r = case bdr_reg r of Reg a -> a
