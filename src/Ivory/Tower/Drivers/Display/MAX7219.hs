{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.Tower.Drivers.Display.MAX7219 where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.Driver.SPI

import Ivory.Tower.Drivers.Display.Fonts

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

    handler dispChan "max7219disp" $ do
      req_e <- emitter req_c 1
      callback $ \val -> do

        -- store dispVal and send dummy message so coroutine handler can do the rest
        refCopy dispVal val

        x <- local $ istruct
          [ tx_device .= ival spiDev
          , tx_buf .= iarray ( map ival [0, 0] )
          , tx_len .= ival 2 ]

        emit req_e $ constRef x

    coroutineHandler initChan res_c "max7219coro" $ do
      req_e <- emitter req_c 1
      return $ CoroutineBody $ \ yield -> do
        let rpc reg val = do
              x <- local $ istruct
                [ tx_device .= ival spiDev
                , tx_buf .= iarray ( map ival [ reg, val ] )
                , tx_len .= ival 2 ]

              emit req_e $ constRef x
              _ <- yield
              return ()

        rpc regShutdown    0x01
        rpc regDecodeMode  0x00
        rpc regScanLimit   0x07
        rpc regIntensity   0x0F
        rpc regDisplayTest 0x00
        rpc 0x01 $ font7seg '°'
        rpc 0x02 $ font7seg '8'
        rpc 0x03 $ font7seg '4'
        rpc 0x04 0x00
        rpc 0x05 $ font7seg 'E'
        rpc 0x06 $ font7seg 'S'
        rpc 0x07 $ font7seg 'A'
        rpc 0x08 $ font7seg 'B'

        forever $ do
          _ <- yield

          len <- dispVal ~>* stringLengthL
          arrayMap $ \(ix :: Ix 8) -> do
              x <- deref (dispVal ~> stringDataL ! (toIx . fromIx $ ix))
              c <- ifte (fromIx ix >=? len) (return $ font7seg ' ') (font7seg' x)
              noBreak $ rpc (8 - ixToU8 ix) (c)
              return ()

          return ()


  return ()
