module Main where

import Ivory.Tower.Config
import Ivory.Tower.Options
import Ivory.OS.FreeRTOS.Tower.STM32

import TestPlatforms
import Ivory.Tower.Drivers.ADC.HX711

import Ivory.Tower
import Ivory.Tower.Base
import Ivory.BSP.STM32.ClockConfig

app :: (e -> ClockConfig)
    -> (e -> ColoredLEDs)
    -> (e -> HX711)
    -> Tower e ()
app tocc toleds tohx = do
  leds <- fmap toleds getEnv
  hx <- fmap tohx getEnv

  blink (Milliseconds 200) [greenLED leds]
  blink (Milliseconds 666) [orangeLED leds]
  blink (Milliseconds 1000) [redLED leds]

  blink (Milliseconds 2000) [blueLED leds]

  --out <- hx711ABTower hx
  out <- hx711ABTower hx
  return ()

main :: IO ()
main = compileTowerSTM32FreeRTOS testplatform_stm32 p $
    app (stm32config_clock . testplatform_stm32)
        testplatform_leds
        testplatform_hx711
  where
  p topts = getConfig topts testPlatformParser
