{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Ivory.Tower.Drivers.Display.MAX7219 where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.Bus.SPI
import Ivory.Tower.HAL.Bus.SPI.DeviceHandle

import Ivory.Tower.Drivers.Display.Fonts

import Ivory.Base (ixToU8)
import Ivory.Tower.Base.UART (isChar)

regDecodeMode, regIntensity, regScanLimit
  , regShutdown, regDisplayTest :: Uint8
regDecodeMode  = 0x09
regIntensity   = 0x0A
regScanLimit   = 0x0B
regShutdown    = 0x0C
regDisplayTest = 0x0F


-- 7 segment display driver using max7219
max7219 :: forall buf e . (IvoryString buf)
        =>  BackpressureTransmit
              ('Struct "spi_transaction_request")
              ('Struct "spi_transaction_result")
        -> SPIDeviceHandle
        -> Proxy buf
        -> Tower e (ChanInput buf, ChanInput ('Stored Uint8))
max7219 (BackpressureTransmit req_c res_c) spiDev _proxybuf = do

  intensityChan <- channel
  dispChan <- channel

  delayedInit <- channel
  p <- period (Milliseconds 100)

  monitor "max7219" $ do
    sentInit <- state "sent"
    delayCycles <- stateInit "dc" (ival (5 :: Uint8))

    handler p "delayInit" $ do
      (d :: Emitter ('Stored IBool)) <- emitter (fst delayedInit) 1
      callback $ const $ do
        sent <- deref sentInit
        dc <- deref delayCycles

        unless sent $ do
          ifte_ (dc ==? 0)
            (do
              emitV d true
              store sentInit true
            )
            (store delayCycles (dc-1))

    dispVal <- stateInit "dispval" (stringInit "Booting")

    handler (snd dispChan) "max7219disp" $ do
      req_e <- emitter req_c 1
      callback $ \val -> do

        -- store dispVal and send dummy message so coroutine handler can do the rest
        refCopy dispVal val
        collapseComma dispVal

        x <- local $ istruct
          [ tx_device .= ival spiDev
          , tx_buf .= iarray ( map ival [0, 0] )
          , tx_len .= ival 2 ]

        emit req_e $ constRef x

    handler (snd intensityChan) "intensity" $ do
      req_e <- emitter req_c 1
      callbackV $ \val -> do

        let min' a b = (a >? b) ? (b, a)

        x <- local $ istruct
          [ tx_device .= ival spiDev
          , tx_buf .= iarray ( map ival [regIntensity, min' val 0xF] )
          , tx_len .= ival 2 ]

        emit req_e $ constRef x

    coroutineHandler (snd delayedInit) res_c "max7219coro" $ do
      req_e <- emitter req_c 1
      int_e <- emitter (fst intensityChan) 1
      return $ CoroutineBody $ \ yield -> do
        let rpc reg val = do
              x <- local $ istruct
                [ tx_device .= ival spiDev
                , tx_buf .= iarray ( map ival [ reg, val ] )
                , tx_len .= ival 2 ]

              emit req_e $ constRef x
              _ <- yield
              return ()

        arrayMap $ \(_ :: Ix 5) -> noBreak $
          rpc regShutdown    0x01

        rpc regShutdown    0x01
        rpc regDecodeMode  0x00
        rpc regScanLimit   0x07
        rpc regIntensity   0x0F
        rpc regDisplayTest 0x00

        emitV int_e 0xF

        forever $ do
          _ <- yield

          len <- dispVal ~>* stringLengthL
          arrayMap $ \(ix :: Ix 8) -> do
              x <- deref (dispVal ~> stringDataL ! (toIx . fromIx $ ix))
              c <- ifte (fromIx ix >=? len)
                (return $ font7seg ' ')
                (ifte (x >=? 148)
                  (font7seg' (x - 100) >>= \fc -> return $ fc + font7seg '.')
                  (font7seg' x))
              noBreak $ rpc (8 - ixToU8 ix) (c)
              return ()

          return ()
  return (fst dispChan, fst intensityChan)

-- Find first comma and collapse it into previous character
-- by adding +100 to its value
collapseComma :: forall eff s cs str . (
                  GetAlloc eff ~ 'Scope cs,
                  IvoryString str) => Ref s str -> Ivory eff ()
collapseComma str = do
  commaFound <- local (ival false)
  arrayMap $ \i -> do

    c <- deref (str ~> stringDataL ! i)

    found <- deref commaFound
    when (c `isChar` '.' .&& i /=? 0 .&& iNot found)
      (do
        let prevIdx = toIx $ (fromIx i) - 1
        prev <- deref (str ~> stringDataL ! prevIdx)
        store (str ~> stringDataL ! prevIdx) $ prev + 100
        store commaFound true
      )

    found' <- deref commaFound
    when found' $
      (do
        next <- deref (str ~> stringDataL ! (toIx $ (fromIx i) + 1))
        store (str ~> stringDataL ! i) next
      )
