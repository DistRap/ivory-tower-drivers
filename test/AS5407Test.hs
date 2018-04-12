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

import Ivory.Tower.Drivers.Encoder.AS5407

app :: (e -> ClockConfig)
    -> (e -> ColoredLEDs)
    -> (e -> TestUART)
    -> (e -> TestSPI)
    -> Tower e ()
app tocc toleds touart2 tospi = do
  leds <- fmap toleds getEnv
  uart2 <- fmap touart2 getEnv
  spi <- fmap tospi getEnv

  uartTowerDeps
  blink (Milliseconds 250) [redLED leds]
  blink (Milliseconds 333) [blueLED leds]

  let devices = [ as5407dev (testSPIPeriph spi) ]

  (sreq, sready) <- spiTower tocc devices (testSPIPins spi)
  (ostream, istream) <- bufferedUartTower tocc (testUARTPeriph uart2) (testUARTPins uart2) 115200 (Proxy :: Proxy UARTBuffer)

  monitor "asInit" $ do
    handler systemInit "init" $ do
      o <- emitter ostream 32
      callback $ const $ do
        ledSetup $ greenLED leds
        ledSetup $ orangeLED leds
        ledSetup $ redLED leds
        ledSetup $ blueLED leds
        puts o "HELO\r\n"

  (asTask0, asReq0) <- task "as5407t"
  as5407 asReq0 (SPIDeviceHandle 0) systemInit

  schedule "as5407s" [asTask0] sready sreq

main :: IO ()
main = compileTowerSTM32FreeRTOS testplatform_stm32 p $
        app (stm32config_clock . testplatform_stm32)
            testplatform_leds
            testplatform_uart2
            testplatform_spi
  where
  p topts = getConfig topts testPlatformParser
