{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}

module Ivory.Tower.Drivers.DAC.DACx1408
  ( DACChan(..)
  , dacx1408Tower
  ) where

import Control.Monad (forM_)
import Prelude hiding (read)
import Ivory.Language
import Ivory.HW.Module
import Ivory.HW.BitData
import Ivory.HW.Reg
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.Bus.SPI
import Ivory.Tower.HAL.Bus.SPI.DeviceHandle

import Ivory.Tower.Drivers.DAC.DACx1408.Peripheral
import Ivory.Tower.Drivers.DAC.DACx1408.Regs
import Ivory.Tower.Drivers.DAC.DACx1408.RegTypes

data DACChan = DACChan
  { dacReady  :: ChanOutput (Stored IBool)
  , dacOutput :: ChanInput (Array 4 (Stored Sint16))
  , dacOffset :: ChanInput (Array 4 (Stored Sint8))
  }

-- | DacX1408 driver (DAC81408, DAC71408, DAC61408)
-- using differential outputs with +-2.5V range
dacx1408Tower
  :: BackpressureTransmit
       ('Struct "spi_transaction_request")
       ('Struct "spi_transaction_result")
  -> ChanOutput ('Stored ITime)
  -> SPIDeviceHandle
  -> Tower
       e
       DACChan
dacx1408Tower (BackpressureTransmit req res) rdy spiDev = do
  let DACx1408{..} = dacx1408

  (dacReadyIn, dacReady) <- channel
  (dacOutput, dacOutputOut) <- channel
  (dacOffset, dacOffsetOut) <- channel

  monitor "dacx1408" $ do
    monitorModuleDef $ hw_moduledef

    spiReq <- stateInit "spiReq" (istruct [ tx_device .= ival spiDev ])
    devID <- state "devID"

    coroutineHandler rdy res "dacCoro" $ do
      reqE <- emitter req 1
      dacReadyE <- emitter dacReadyIn 1

      return $ CoroutineBody $ \yield -> do

        let rpc createReq = do
              createReq spiReq
              emit reqE (constRef spiReq)
              x <- yield
              return x

            read x = do
              -- set register we are reading
              _ <- rpc x
              -- actually read data
              result <- rpc x
              hi <- deref (result ~> rx_buf ! 1)
              lo <- deref (result ~> rx_buf ! 2)
              pure (safeCast hi `iShiftL` 8 .| safeCast lo)

            write x = rpc x >>= const (return ())

        write
          $ dacWrite
              dacSPI
              (repToBits $ withBits 0 $ do
                setBit spi_sdo_enable
              )

        dacID <- read $ dacRead dacDeviceID
        let dev = fromRep dacID #. device_id_data
        store devID dev
        assert
          (   dev ==? device_dac81408
          .|| dev ==? device_dac71408
          .|| dev ==? device_dac61408
          )

        forM_ [dacRange0, dacRange1] $ \rangeReg ->
          write
            $ dacWrite
                rangeReg
                (repToBits $ withBits 0 $ do
                  setField range_dac_a range_minus2point5to2point5volts
                  setField range_dac_b range_minus2point5to2point5volts
                  setField range_dac_c range_minus2point5to2point5volts
                  setField range_dac_d range_minus2point5to2point5volts
                )

        -- Power down internal reference, differential mode
        write
          $ dacWrite
              dacGeneral
              (repToBits $ withBits 0 $ do
                setBit general_ref_powerdown
                setBit general_dac0and1_differential
                setBit general_dac2and3_differential
                setBit general_dac4and5_differential
                setBit general_dac6and7_differential
              )

        -- Zero all channels
        forM_ [ dac0, dac1, dac2, dac3, dac4, dac5, dac6, dac7 ] $ \reg ->
          write
            $ dacWrite
                reg
                $ fromRep 0x7FFF

        -- Enable outputs
        write
          $ dacWrite
              dacPowerDown
              $ fromRep 0

        -- Enable streaming
        --
        -- Note that reads won't work after
        -- this point without
        -- disabling streaming again
        spi <- read $ dacRead dacSPI
        write
          $ dacWrite
              dacSPI
              (repToBits $ withBits spi $ do
                setBit spi_streaming_enable
              )

        emitV dacReadyE true

        -- Emit ready on each response,
        -- triggered by handlers bellow
        forever $ do
          _ <- yield
          emitV dacReadyE true

    handler dacOutputOut "dacStreamingSetter" $ do
      reqE <- emitter req 1
      callback $ \arr -> do
        -- Prepare streaming write, starting with dacOut0 register
        dacWriteOutputs
          dac0
          arr
          spiReq

        emit reqE (constRef spiReq)

    handler dacOffsetOut "dacStreamingOffset" $ do
      reqE <- emitter req 1
      callback $ \arr -> do
        -- Prepare streaming write, starting with dacOff0 register
        off67 <- deref (arr ! 3)
        off45 <- deref (arr ! 2)
        off23 <- deref (arr ! 1)
        off01 <- deref (arr ! 0)

        dacWriteOffsets
          offset0
          (repToBits $ withBits 0 $ do
            setField offset_ab (fromRep $ twosComplementRep off67)
            setField offset_cd (fromRep $ twosComplementRep off45)
          )
          (repToBits $ withBits 0 $ do
            setField offset_ab (fromRep $ twosComplementRep off23)
            setField offset_cd (fromRep $ twosComplementRep off01)
          )
          spiReq

        emit reqE (constRef spiReq)

  pure DACChan {..}

convertOutput :: Sint16 -> Uint16
convertOutput =
    (bitCast :: Uint32 -> Uint16)
  . (signCast :: Sint32 -> Uint32)
  . (+0x7FFF)
  . (safeCast :: Sint16 -> Sint32)

dacRead
  :: BitDataReg a
  -> Ref s ('Struct "spi_transaction_request")
  -> Ivory eff ()
dacRead bdr r = do
  store (r ~> tx_buf ! 0) (0x80 .| (fromIntegral $ regAddr bdr))

dacWrite
  :: BitDataReg a
  -> Bits 16
  -> Ref s ('Struct "spi_transaction_request")
  -> Ivory eff ()
dacWrite bdr dat r = do
  store (r ~> tx_buf ! 0) (fromIntegral $ regAddr bdr)
  store (r ~> tx_buf ! 1) (bitCast $ toRep dat `iShiftR` 8)
  store (r ~> tx_buf ! 2) (bitCast $ toRep dat)
  store (r ~> tx_len) 3

-- | Construct a write for all DAC outputs
-- skipping even registers since their values are ignored
-- in differential mode (but has to be written to due to
-- how streaming mode works)
dacWriteOutputs
  :: BitDataReg a
  -> ConstRef s1 (Array 4 (Stored Sint16))
  -> Ref s2 ('Struct "spi_transaction_request")
  -> Ivory eff ()
dacWriteOutputs bdr arr r = do
  store (r ~> tx_buf ! 0) (fromIntegral $ regAddr bdr)
  arrayMap $ \i ->
    deref (arr ! i)
    >>= setOutput ((toIx . fromIx) i) . convertOutput

  store (r ~> tx_len) (8*2 + 1)
  where
    setOutput i val = do
      store (r ~> tx_buf ! (i * 4 + 1)) (bitCast $ val `iShiftR` 8)
      store (r ~> tx_buf ! (i * 4 + 2)) (bitCast $ val)

-- | Construct a write for all DAC offsets
dacWriteOffsets
  :: BitDataReg a
  -> Bits 16
  -> Bits 16
  -> Ref s ('Struct "spi_transaction_request")
  -> Ivory eff ()
dacWriteOffsets bdr off0 off1 r = do
  store (r ~> tx_buf ! 0) (fromIntegral $ regAddr bdr)
  store (r ~> tx_buf ! 1) (bitCast $ toRep off0 `iShiftR` 8)
  store (r ~> tx_buf ! 2) (bitCast $ toRep off0)
  store (r ~> tx_buf ! 3) (bitCast $ toRep off1 `iShiftR` 8)
  store (r ~> tx_buf ! 4) (bitCast $ toRep off1)
  store (r ~> tx_len) 5

regAddr :: BitDataReg d -> Integer
regAddr r = case bdr_reg r of Reg a -> a
