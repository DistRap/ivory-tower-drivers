{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Ivory.Tower.Config
import Ivory.OS.FreeRTOS.Tower.STM32

import TestPlatforms
import Ivory.Language
import Ivory.Stdlib
import Ivory.Serialize.LittleEndian
import Ivory.Tower
import Ivory.Tower.Base
import Ivory.Tower.Base.UART.Types
import Ivory.Tower.Base.Util
import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Driver.I2C

import Ivory.Tower.Drivers.Net.RN2483
import Ivory.Tower.Drivers.Sensor.SHT21

app :: (e -> ClockConfig)
    -> (e -> ColoredLEDs)
    -> (e -> TestUART)
    -> (e -> TestUART)
    -> (e -> TestI2C)
    -> Tower e ()
app tocc toleds touart2 touart3 toi2c = do
  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts

  leds <- fmap toleds getEnv
  uart2 <- fmap touart2 getEnv
  uart3 <- fmap touart3 getEnv

  uartTowerDeps

  (ostream, istream) <- bufferedUartTower tocc (testUARTPeriph uart2) (testUARTPins uart2) 115200 (Proxy :: Proxy UARTBuffer)
  (ostream', istream') <- bufferedUartTower tocc (testUARTPeriph uart3) (testUARTPins uart3) 57600 (Proxy :: Proxy UARTBuffer)

  i2c <- fmap toi2c getEnv
  (BackpressureTransmit i2c_req i2c_res, _ready) <- i2cTower tocc (testI2C i2c) (testI2CPins i2c)
  (temp_out, hum_out) <- sht21Tower (BackpressureTransmit i2c_req i2c_res)
  _ <- toggleOnChanTower temp_out (orangeLED leds)

  istreamf' <- replaceChar '\r' '\n' istream' >>= dropEvery 2 (`isChar` '\n')
  uartBridge ostream istream ostream' istreamf'

  dbgstream <- dbgPrepend '\n' "> " ostream
  merged <- ostream' `mergeInputs` dbgstream

  periodic <- period (Milliseconds 60000)

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
  (rdy, acc, txdone, cmd) <- rn2483 merged istreamCRLF systemInit resetPin (Proxy :: Proxy UARTBuffer)
  _ <- toggleOnChanTower txdone (blueLED leds)

  monitor "rnleds" $ do
  --monitor "shtsensor" $ do
    lasttemp <- state "lasttemp"
    lasthum <- state "lasthum"
    lastx <- state "lastx"
    ready <- state "ready"

    handler rdy "rnRdy" $ do
      callback $ const $ do
        ledOn $ orangeLED leds

    handler acc "rnAccepted" $ do
      cmdE <- emitter cmd 1
      callbackV $ \v -> do
        when v $ do
          ledOn $ greenLED leds
          store ready true

        unless v $ ledOn $ redLED leds

        x <- local (izero :: Init UARTBuffer)
        packInto (x ~> stringDataL) 0 (constRef lasttemp)
        packInto (x ~> stringDataL) 4 (constRef lasthum)
        store (x ~> stringLengthL) 8
        refCopy lastx x
        r <- deref ready
        when r $ do
          emit cmdE (constRef x)

    handler temp_out "temperature" $ do
      callback $ \rref -> do
        refCopy lasttemp rref

    handler hum_out "humidity" $ do
      callback $ \rref -> do
        refCopy lasthum rref

    handler periodic "periodic" $ do
      cmdE <- emitter cmd 1
      callback $ const $ do
        x <- local (izero :: Init UARTBuffer)
        packInto (x ~> stringDataL) 0 (constRef lasttemp)
        packInto (x ~> stringDataL) 4 (constRef lasthum)
        store (x ~> stringLengthL) 8
        refCopy lastx x
        r <- deref ready
        when r $ do
          emit cmdE (constRef x)

main :: IO ()
main = compileTowerSTM32FreeRTOS testplatform_stm32 p $
        app (stm32config_clock . testplatform_stm32)
            testplatform_leds
            testplatform_uart2
            testplatform_uart3
            testplatform_i2c
  where
  p topts = getConfig topts testPlatformParser
