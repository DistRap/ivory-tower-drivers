{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Ivory.Tower.Config
import Ivory.OS.FreeRTOS.Tower.STM32

import TestPlatforms
import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.Base
import Ivory.Tower.Base.UART.Types
import Ivory.Tower.Base.Util

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.SPI
import Ivory.BSP.STM32.Peripheral.SPI

import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.Bus.Sched

import Ivory.Tower.Drivers.Display.MAX7219Matrix

app :: (e -> ClockConfig)
    -> (e -> ColoredLEDs)
    -> (e -> TestUART)
    -> (e -> TestSPI)
    -> Tower e ()
app tocc toleds touart2 tospi = do
  leds <- fmap toleds getEnv
  uart2 <- fmap touart2 getEnv
  spi <- fmap tospi getEnv

  blink (Milliseconds 200) [greenLED leds]
  blink (Milliseconds 666) [orangeLED leds]
  blink (Milliseconds 1000) [redLED leds]

  blink (Milliseconds 2000) [blueLED leds]

  uartTowerDeps

  let devices = [ max7219dev (testSPIPeriph spi), max7219dev2 (testSPIPeriph spi)]
  --let devices = [ max7219dev (testSPIPeriph spi) ]


  (sreq, sready) <- spiTower tocc devices (testSPIPins spi)
  (ostream, istream) <- bufferedUartTower tocc (testUARTPeriph uart2) (testUARTPins uart2) 115200 (Proxy :: Proxy UARTBuffer)

  monitor "maxInit" $ do
    handler systemInit "init" $ do
      o <- emitter ostream 32
      callback $ const $ do
        ledSetup $ greenLED leds
        ledSetup $ orangeLED leds
        ledSetup $ redLED leds
        ledSetup $ blueLED leds
        puts o "HELO\r\n"

  (xIn, xOut) <- channel

  (maxTask0, maxReq0) <- task "max7219_0"
  max7219 maxReq0 (SPIDeviceHandle 0) systemInit xOut (Proxy :: Proxy UARTBuffer)

  (maxTask1, maxReq1) <- task "max7219_1"
  max7219 maxReq1 (SPIDeviceHandle 1) systemInit xOut (Proxy :: Proxy UARTBuffer)

  schedule "maxes" [maxTask0, maxTask1] sready sreq
  --schedule "maxes" [maxTask0] sready sreq

  p <- period (Milliseconds 2000)
  monitor "a" $ do
    handler p "ap" $ do
      o <- emitter xIn 1
      callback $ const $ do
        x <- local $ stringInit "base"
        emit o $ constRef x

main :: IO ()
main = compileTowerSTM32FreeRTOS testplatform_stm32 p $
        app (stm32config_clock . testplatform_stm32)
            testplatform_leds
            testplatform_uart2
            testplatform_spi
  where
  p topts = getConfig topts testPlatformParser
