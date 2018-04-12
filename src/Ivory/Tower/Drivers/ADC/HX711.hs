{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Ivory.Tower.Drivers.ADC.HX711 where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.HW

import Ivory.BSP.STM32.Peripheral.GPIOF4

data HX711 =
  HX711
    { clockPin :: GPIOPin
    , dataPin  :: GPIOPin
    }


-- simple version reading A channel at 128x gain
hx711Tower :: HX711 -> Tower e (ChanOutput ('Stored Uint32))
hx711Tower HX711{clockPin=clk, dataPin=dat} = do

  periodic <- period (Milliseconds 500)

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
          datValue <- pinRead dat
          when (datValue ==? false) breakOut

        let pinSetHold = arrayMap $ \(_ :: Ix 4) -> pinSet clk
            pinClearHold = arrayMap $ \(_ :: Ix 4) -> pinClear clk

        comment "sample data"
        arrayMap $ \(i :: Ix 24) -> do
          pinSetHold

          val <- (pinRead dat)
          currenterWeight <- deref sampledVal
          store sampledVal $ currenterWeight  .| ((safeCast val) `iShiftL` (24 - signCast (fromIx i)))

          pinClearHold

        comment "get chan A at 128 on next cycle"
        arrayMap $ \(_ :: Ix 1) -> do
          pinSetHold
          pinClearHold

        emit e (constRef sampledVal)

    handler systemInit "init" $ do
      callback $ const $ do
        set_output_pin clk
        set_input_pin dat

  return (snd chan)

 -- version reading A channel at 64x gain and channel B at 32x gain
hx711ABTower :: HX711
           -> Tower e (ChanOutput ('Stored Uint32), ChanOutput ('Stored Uint32))
hx711ABTower HX711{clockPin=clk, dataPin=dat} = do

  periodic <- period (Milliseconds 500)

  chanA <- channel
  chanB <- channel

  monitor "hx711" $ do
    monitorModuleDef $ hw_moduledef

    expectChanA <- state "expectChanA"
    sampledVal <- stateInit "sampledVal" (ival (0 :: Uint32))
    sampledValA <- stateInit "sampledValA" (ival (0 :: Uint32))
    sampledValB <- stateInit "sampledValB" (ival (0 :: Uint32))
    cnt <- stateInit "cnt" (ival (0 :: Uint32))

    handler periodic "periodic_hx711" $ do
      eA <- emitter (fst chanA) 1
      eB <- emitter (fst chanB) 1
      callback $ const $ do

        let pinSetHold = arrayMap $ \(_ :: Ix 4) -> pinSet clk
            pinClearHold = arrayMap $ \(_ :: Ix 4) -> pinClear clk

        store sampledVal 0

        pinClearHold
        comment "wait for converter to settle for result"
        forever $ do
          cnt += 1
          datValue <- pinRead dat
          when (datValue ==? false) breakOut


        comment "sample data"
        arrayMap $ \(i :: Ix 24) -> noBreak $ do
          pinSetHold

          val <- (pinRead dat)
          currenterWeight <- deref sampledVal
          store sampledVal $ currenterWeight  .| ((safeCast val) `iShiftL` (24 - signCast (fromIx i)))

          pinClearHold

        aExpected <- deref expectChanA
        ifte_ (aExpected)
          (do
             comment "get chan B at 32 on next cycle"
             arrayMap $ \(_ :: Ix 2) -> noBreak $ do
               pinSetHold
               pinClearHold

             refCopy sampledValA sampledVal
             emit eA (constRef sampledValA)

             store expectChanA false
          )
          (do
             comment "get chan A at 64 on next cycle"
             arrayMap $ \(_ :: Ix 3) -> noBreak $ do
               pinSetHold
               pinClearHold

             refCopy sampledValB sampledVal
             emit eB (constRef sampledValB)

             store expectChanA true
          )


    handler systemInit "init" $ do
      callback $ const $ do
        set_output_pin clk
        set_input_pin dat

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

