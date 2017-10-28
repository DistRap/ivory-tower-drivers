module Main where

import TestPlatforms
import Ivory.Language
import Ivory.Stdlib
import Ivory.Serialize
import Ivory.Tower
import Ivory.Tower.Base
import Ivory.Tower.Base.UART.Types
import Ivory.Tower.Config
import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.Driver.I2C
import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.ClockConfig

import Ivory.Tower.Drivers.Sensor.SHT21

import Ivory.OS.FreeRTOS.Tower.STM32

app :: (e -> ClockConfig)
    -> (e -> TestI2C)
    -> (e -> ColoredLEDs)
    -> (e -> TestUART)
    -> Tower e ()
app tocc totesti2c toleds touart = do
  towerDepends uartTestTypes
  towerModule  uartTestTypes
  towerDepends serializeModule
  towerModule  serializeModule
  mapM_ towerArtifact serializeArtifacts

  i2c <- fmap totesti2c getEnv
  (BackpressureTransmit req res, _ready) <- i2cTower tocc (testI2C i2c) (testI2CPins i2c)

  e <- getEnv
  (red_led_in, red_led_out) <- channel
  (blue_led_in, blue_led_out) <- channel
  monitor "settableLEDR" $ ledController [redLED (toleds e)] red_led_out
  monitor "settableLEDB" $ ledController [blueLED (toleds e)] blue_led_out

  u <- fmap touart getEnv
  (ostream, _ostream, mon) <- uartTower tocc (testUARTPeriph u) (testUARTPins u) 115200
  monitor "dma" mon

  (temp_out, hum_out) <- sht21Tower (BackpressureTransmit req res)

  monitor "simplecontroller" $ do
    lasttemp <- state "lasttemp"
    lastx <- state "lastx"
    pending <- state "pending"
    handler temp_out "temperature" $ do
      led_emitter_r <- emitter red_led_in 1
      led_emitter_b <- emitter blue_led_in 1
      uartemitter <- emitter (backpressureTransmit ostream) 1
      callback $ \rref -> do

        temp <- deref rref
        l <- deref lasttemp
        ifte_ (temp >? l)
          (emitV led_emitter_b false >> emitV led_emitter_r true)
          (emitV led_emitter_b true >> emitV led_emitter_r false)
        refCopy lasttemp rref
        when (iNot $ isnan temp) $ do
          --assert (isnan temp)
          (emitV led_emitter_b true >> emitV led_emitter_r false)

        x <- local (izero :: Init UARTBuffer)
        packInto (x ~> stringDataL) 0 rref
        store (x ~> stringLengthL) 5
        store (x ~> stringDataL ! 4) (10 :: Uint8)
        refCopy lastx x

        p <- deref pending
        unless p $ do
          emit uartemitter (constRef x)
          store pending true

    handler (backpressureComplete ostream) "complete" $ do
      callback $ const $ do
        store pending false

uartTestTypes :: Module
uartTestTypes = package "uartTestTypes" $ do
  defStringType (Proxy :: Proxy UARTBuffer)

main :: IO ()
main = compileTowerSTM32FreeRTOS testplatform_stm32 p $
        app (stm32config_clock . testplatform_stm32)
            testplatform_i2c testplatform_leds testplatform_uart2
  where
  p topts = getConfig topts testPlatformParser
