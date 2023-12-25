{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}

module Ivory.Tower.Drivers.ADC.HX711 where

import Control.Monad (void)
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.HW

import Ivory.BSP.STM32.Peripheral.GPIO

data AGain = AGain128 | AGain64
data BGain = BGain32

data HX711 =
  HX711
    { clockPin :: GPIOPin
    , dataPin  :: GPIOPin
    , fastMode :: Bool
    , channelAGain :: Maybe AGain
    , channelBGain :: Maybe BGain
    }

fastSampleRate False = hertzToMicroseconds 10
fastSampleRate True = hertzToMicroseconds 80

hertzToMicroseconds :: Integer -> Microseconds
hertzToMicroseconds hz = Microseconds . round $ 1 / (fromInteger hz) * 1_000_000

-- Simple version reading A channel at 128x gain
hx711Tower
  :: HX711
  -> Tower e (ChanOutput ('Stored Sint32))
hx711Tower HX711{..} = do
  periodic <- period (fastSampleRate fastMode)

  chan <- channel

  monitor "hx711" $ do
    monitorModuleDef $ hw_moduledef

    sampledVal <- stateInit "sampledVal" (ival (0 :: Uint32))

    handler periodic "periodic_hx711" $ do
      e <- emitter (fst chan) 1
      callback $ const $ do
        store sampledVal 0

        comment "wait for converter to settle for result"
        forever $ do
          datValue <- pinRead dataPin
          when (datValue ==? false) breakOut

        let pinSetHold = arrayMap $ \(_ :: Ix 4) -> pinSet clockPin
            pinClearHold = arrayMap $ \(_ :: Ix 4) -> pinClear clockPin

        comment "sample data"
        arrayMap $ \(i :: Ix 24) -> do
          pinSetHold

          val <- pinRead dataPin
          currenterWeight <- deref sampledVal
          store sampledVal $ currenterWeight  .| ((safeCast val) `iShiftL` (24 - signCast (fromIx i)))

          pinClearHold

        comment "get chan A at 128 on next cycle"
        arrayMap $ \(_ :: Ix 1) -> do
          pinSetHold
          pinClearHold

        sintVal <- twosComplementCast <$> deref sampledVal
        emitV e sintVal

    handler systemInit "init" $ do
      callback $ const $ do
        set_output_pin clockPin
        set_input_pin dataPin

  return (snd chan)

-- Version reading A channel at 64x gain and channel B at 32x gain
hx711ABTower
  :: HX711
  -> Tower e (ChanOutput ('Stored Sint32), ChanOutput ('Stored Sint32))
hx711ABTower HX711{..} = do

  periodic <- period (fastSampleRate fastMode)

  chanA <- channel
  chanB <- channel

  monitor "hx711" $ do
    monitorModuleDef $ hw_moduledef

    expectChanA <- state "expectChanA"
    sampledVal <- stateInit "sampledVal" (ival (0 :: Uint32))
    sampledValA <- stateInit "sampledValA" (ival (0 :: Uint32))
    sampledValB <- stateInit "sampledValB" (ival (0 :: Uint32))
    cnt <- stateInit "cnt" (ival (0 :: Uint32))

    ms <- state "mswat"
    correctedVal <- stateInit "correctedVal" (ival (0 :: Uint32))

    handler periodic "periodic_hx711" $ do
      eA <- emitter (fst chanA) 1
      eB <- emitter (fst chanB) 1
      callback $ const $ do

        let pinSetHold = arrayMap $ \(_ :: Ix 2) -> pinSet clockPin
            pinClearHold = arrayMap $ \(_ :: Ix 2) -> pinClear clockPin

        store sampledVal 0

        pinClearHold
        comment "wait for converter to settle for result"
        forever $ do
          cnt += 1
          datValue <- pinRead dataPin
          when (datValue ==? false) breakOut

        comment "sample data"
        arrayMap $ \(i :: Ix 24) -> noBreak $ do
          pinSetHold

          val <- pinRead dataPin
          currenterWeight <- deref sampledVal
          store sampledVal $ currenterWeight  .| ((safeCast val) `iShiftL` (24 - signCast (fromIx $ i)))

          pinClearHold

        -- Pad with 0xFF if sampled MSB is 1 (24bit twos complement)
        msb <- (==? 1) . (`iShiftR` 23) . (0x800000 .&) <$> deref sampledVal
        store ms msb

        aExpected <- deref expectChanA
        ifte_ (aExpected)
          (do
             comment "get chan B at 32 on next cycle"
             arrayMap $ \(_ :: Ix 2) -> noBreak $ do
               pinSetHold
               pinClearHold

             refCopy sampledValA sampledVal

             sintVal <- twosComplementCast <$> deref sampledValA
             emitV eA sintVal

             store expectChanA false
          )
          (do
             comment "get chan A at 64 on next cycle"
             arrayMap $ \(_ :: Ix 3) -> noBreak $ do
               pinSetHold
               pinClearHold

             refCopy sampledValB sampledVal

             sintVal <- deref sampledValB

             emitV eB $ ((twosComplementCast :: Uint32 -> Sint32) (sintVal `iShiftL` 8)) `iDiv` 256

             store expectChanA true
          )


    handler systemInit "init" $ do
      callback $ const $ do
        set_output_pin clockPin
        set_input_pin dataPin

  return (snd chanA, snd chanB)

 -- | Coroutine version reading A channel at 64x gain and channel B at 32x gain
hx711ABC
  :: HX711
  -> Tower e (ChanOutput ('Stored Sint32), ChanOutput ('Stored Sint32))
hx711ABC HX711{..} = do

  periodic <- period (fastSampleRate fastMode)
  -- maximum reset cycle length before we try again
  initPeriod <- period (Milliseconds 100)

  chanA <- channel
  chanB <- channel
  rdy <- channel

  monitor "hx711" $ do
    monitorModuleDef $ hw_moduledef

    expectChanA <- stateInit "expectChanA" (ival false)

    sampledVal <- stateInit "sampledVal" (ival (0 :: Uint32))
    sampledValA <- stateInit "sampledValA" (ival (0 :: Uint32))
    sampledValB <- stateInit "sampledValB" (ival (0 :: Uint32))
    cnt <- stateInit "cnt" (ival (0 :: Uint32))
    cnt2 <- stateInit "cnt2" (ival (0 :: Uint32))

    pulseCount <- stateInit "pc" (ival (0 :: Uint32))
    resetCount <- stateInit "rc" (ival (0 :: Uint32))

    coroutineHandler systemInit periodic "periodic_hx711" $ do
      eA <- emitter (fst chanA) 1
      eB <- emitter (fst chanB) 1
      ready <- emitter (fst rdy) 1
      return $ CoroutineBody $ \yield -> do

        let pinSetHold = arrayMap $ \(_ :: Ix 4) -> pinSet clockPin
            pinClearHold = arrayMap $ \(_ :: Ix 4) -> pinClear clockPin
            cycle = pinSetHold >> pinClearHold

        comment "perform reset"
        pinSet clockPin
        -- wait +10ms
        forever $ do
          resetCount += 1
          rc <- deref resetCount
          when (rc >=? 10) breakOut
          void $ yield
        pinClear clockPin

        comment "wait for converter to settle for result"
        forever $ do
          cnt += 1
          datValue <- pinRead dataPin
          when (datValue ==? false) breakOut
          void $ yield

        emitV ready true
        forever $ do
          store sampledVal 0

          forever $ do
            cnt2 += 1
            pinClear clockPin
            datValuex <- pinRead dataPin
            when (datValuex ==? false) breakOut
            void $ yield

          comment "sample data"
          arrayMap $ \(i :: Ix 24) -> do
            pinSetHold

            val <- pinRead dataPin
            currenterWeight <- deref sampledVal
            store sampledVal $ currenterWeight  .| ((safeCast val) `iShiftL` (24 - signCast (fromIx $ i)))

            pinClearHold

          case (channelAGain, channelBGain) of
            (Nothing, Nothing) -> error "No channel to sample, set either channelAGain or channelBGain"
            (Just aGain, Nothing) -> do
              comment "repeated A"
            (Nothing, Just _) -> do

              comment "repeated B"
              comment "get chan B at 32 on next cycle"
              arrayMap $ \(_ :: Ix 2) -> do
                pinSetHold
                pinClearHold

              sintVal <- deref sampledVal
              emitV eB $ ((twosComplementCast :: Uint32 -> Sint32) (sintVal `iShiftL` 8)) `iDiv` 256

            (Just aGain, Just _) -> do

              comment "both channels, half A half B"
              aExpected <- deref expectChanA
              ifte_ (aExpected)
                (do
                  comment "get chan B at gain 32 on next cycle"
                  store pulseCount 0
                  arrayMap $ \(_ :: Ix 2) -> do
                    pinSetHold
                    pinClearHold
                    pulseCount += 1

                  refCopy sampledValA sampledVal

                  sintVal <- deref sampledValA
                  emitV eA $
                    ((twosComplementCast :: Uint32 -> Sint32)
                       (sintVal `iShiftL` 8))
                    `iDiv` 256

                  store expectChanA false
                )
                (do
                  case channelAGain of
                    Just AGain128 -> do
                      comment "get chan A at 128 gain on next cycle"
                      pinSetHold
                      pinClearHold
                    Just AGain64 -> do
                      comment "get chan A at 64 gain on next cycle"
                      arrayMap $ \(_ :: Ix 3) -> do
                        pinSetHold
                        pinClearHold
                    Nothing -> comment "no channel A sampling"

                  refCopy sampledValB sampledVal

                  sintVal <- deref sampledValB

                  emitV eB $
                    ((twosComplementCast :: Uint32 -> Sint32)
                        (sintVal `iShiftL` 8))
                    `iDiv` 256

                  store expectChanA true
                )

          comment "wait for next periodic wakeup"
          void $ yield


    handler systemInit "init" $ do
      callback $ const $ do
        set_output_pin clockPin
        set_input_pin dataPin

  return (snd chanA, snd chanB)

set_output_pin :: GPIOPin -> Ivory eff ()
set_output_pin p = do
  pinEnable   p
  pinSetMode  p gpio_mode_output
  pinSetPUPD  p gpio_pupd_pullup
  pinSetSpeed p gpio_speed_2mhz

set_input_pin :: GPIOPin -> Ivory eff ()
set_input_pin p = do
  pinEnable   p
  pinSetMode  p gpio_mode_input
  pinSetPUPD  p gpio_pupd_none
  pinSetSpeed p gpio_speed_2mhz


