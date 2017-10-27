module Main where

import Ivory.Tower.Config
import Ivory.OS.FreeRTOS.Tower.STM32

import TestPlatforms
import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Base
import Ivory.Tower.Base.UART.Types
import Ivory.Tower.Base.Util

import Ivory.BSP.STM32.ClockConfig

import Ivory.Tower.Drivers.Net.RN2483

app :: (e -> ClockConfig)
    -> (e -> ColoredLEDs)
    -> (e -> TestUART)
    -> (e -> TestUART)
    -> Tower e ()
app tocc toleds touart2 touart3 = do
  leds <- fmap toleds getEnv
  uart2 <- fmap touart2 getEnv
  uart3 <- fmap touart3 getEnv

  uartTowerDeps

  (ostream, istream) <- bufferedUartTower tocc (testUARTPeriph uart2) (testUARTPins uart2) 115200 (Proxy :: Proxy UARTBuffer)
  (ostream', istream') <- bufferedUartTower tocc (testUARTPeriph uart3) (testUARTPins uart3) 57600 (Proxy :: Proxy UARTBuffer)

  istreamf' <- replaceChar '\r' '\n' istream' >>= dropEvery 2 (`isChar` '\n')
  uartBridge ostream istream ostream' istreamf'

  dbgstream <- dbgPrepend '\n' "> " ostream
  merged <- ostream' `mergeInputs` dbgstream

  monitor "bridge" $ do
    handler systemInit "init" $ do
      o <- emitter ostream 32
      callback $ const $ do
        ledSetup $ greenLED leds
        ledSetup $ orangeLED leds
        ledSetup $ redLED leds
        ledSetup $ blueLED leds
        puts o "HELO\r\n"

  istreamCRLF <- crlfBuffer istream' (Proxy :: Proxy UARTBuffer)
  (rdy, acc, txdone) <- rn2483 merged istreamCRLF systemInit resetPin (Proxy :: Proxy UARTBuffer)

  monitor "rnleds" $ do
    handler rdy "rnRdy" $ do
      callback $ const $ do
        ledOn $ orangeLED leds

    handler acc "rnAccepted" $ do
      callbackV $ \v -> do
        ifte_ v (ledOn $ greenLED leds) (ledOn $ redLED leds)

    handler txdone "rnTXdone" $ do
      callback $ const $ do
        ledOn $ blueLED leds

main :: IO ()
main = compileTowerSTM32FreeRTOS testplatform_stm32 p $
        app (stm32config_clock . testplatform_stm32)
            testplatform_leds
            testplatform_uart2
            testplatform_uart3
  where
  p topts = getConfig topts testPlatformParser
