{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.Tower.Drivers.Display.MAX7219Matrix where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.Driver.SPI

import Ivory.Tower.Drivers.Display.MatrixFonts

import Ivory.Base (ixToU8)

regDecodeMode = 0x09
regIntensity = 0x0A
regScanLimit = 0x0B
regShutdown = 0x0C
regDisplayTest = 0x0F


max7219 :: forall buf init e . (IvoryString buf, IvoryZero init, IvoryArea init)
        =>  BackpressureTransmit
              ('Struct "spi_transaction_request")
              ('Struct "spi_transaction_result")
        -> SPIDeviceHandle
        -> ChanOutput init
        -> ChanOutput buf
        -> Proxy buf
        -> Tower e ()
max7219 (BackpressureTransmit req_c res_c) spiDev initChan dispChan _proxybuf = do
  monitor "max7219" $ do

    dispVal <- state "dispval"
    font <- stateInit "font" (cp437Font)
    cpfont <- stateInit "font" (cp437Font)
    sfont <- stateInit "font" (sinclairsFont)
    lcdfont <- stateInit "font" (lcdFont)
    tinyfont <- stateInit "font" (tinyFont)

    tmp <- stateInit "tmp" (ival (0 :: Uint8))

    handler dispChan "max7219disp" $ do
      req_e <- emitter req_c 1
      callback $ \val -> do

        -- store dispVal and send dummy message so coroutine handler can do the rest
        refCopy dispVal val

        x <- local $ istruct
          [ tx_device .= ival spiDev
          , tx_buf .= iarray ( map ival [0, 0] )
          , tx_len .= ival 2 ]

        --return ()
        emit req_e $ constRef x

    coroutineHandler initChan res_c "max7219coro" $ do
      req_e <- emitter req_c 1
      return $ CoroutineBody $ \ yield -> do
        let rpc reg val = flip mapM_ [0..3] (pure $ rpc1 reg val)
            rpc1 reg val = do
              x <- local $ istruct
                [ tx_device .= ival spiDev
                , tx_buf .= iarray ( map ival [ reg, val ] )
                , tx_len .= ival 2 ]

              emit req_e $ constRef x
              _ <- yield
              return ()

            rpcx length num reg val = do
              let nops n = take (2 * n) $ repeat 0
              x <- local $ istruct
                [ tx_device .= ival spiDev
                , tx_buf .= iarray ( map ival $ concat $ [ nops (length - 1 - num), [ reg, val ], nops num] )
                , tx_len .= ival 8 ]

              emit req_e $ constRef x
              _ <- yield
              return ()

            rpcx4 = rpcx 4
 
        rpc regShutdown    0x01
        rpc regDecodeMode  0x00
        rpc regScanLimit   0x07
        --rpc regIntensity   0x0F
        rpc regIntensity   0x01
        rpc regDisplayTest 0x00

        let reIx = toIx . fromIx
            nop = rpc 0 0

        arrayMap $ \(ix :: Ix 8) -> do
              c <- deref (font ! 2 ! reIx ix)
              noBreak $ rpc (1 + ixToU8 ix) (c)
              return ()

        forever $ do
          _ <- yield

          ci <- deref tmp
          tmp += 1

          arrayMap $ \(ix :: Ix 8) -> do
--            c <- deref (font ! (toIx ci) ! reIx ix)
--            noBreak $ do
--              flip mapM [0..4] $ \segment -> do
--                rpcx4 segment (1 + ixToU8 ix) (c)
            noBreak $ do
              flip mapM (zip [0..4] [cpfont, sfont, tinyfont, lcdfont]) $ \(segment, f) -> do
                c <- deref (f ! (toIx ci) ! reIx ix)
                rpcx4 segment (1 + ixToU8 ix) (c)
          return ()

  return ()
