{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module TestPlatforms where

import Ivory.Tower.Config
import Data.Char (toUpper)

import qualified Ivory.BSP.STM32F405.CAN         as F405
import qualified Ivory.BSP.STM32F405.UART        as F405
import qualified Ivory.BSP.STM32F405.GPIO        as F405
import qualified Ivory.BSP.STM32F405.GPIO.AF     as F405
import qualified Ivory.BSP.STM32F405.I2C         as F405
import qualified Ivory.BSP.STM32F405.SPI         as F405
import qualified Ivory.BSP.STM32F405.RNG         as F405

import Ivory.BSP.STM32.Peripheral.CAN
import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.Peripheral.UART
import Ivory.BSP.STM32.Peripheral.I2C
import Ivory.BSP.STM32.Peripheral.RNG
import Ivory.BSP.STM32.Peripheral.UART.DMA
import Ivory.BSP.STM32.Peripheral.SPI as SPI
import Ivory.BSP.STM32.ClockConfig
import Ivory.BSP.STM32.Config

import Ivory.Tower.Drivers.ADC.HX711
import qualified Ivory.Tower.Base as Base

testPlatformParser :: ConfigParser TestPlatform
testPlatformParser = do
  p <- subsection "args" $ subsection "platform" string
  case map toUpper p of
    "F4DISCOVERY"       -> result f4discovery
    _ -> fail ("no such platform " ++ p)

  where
  result platform = do
    conf <- stm32ConfigParser (testplatform_stm32 platform)
    return platform { testplatform_stm32 = conf }

data ColoredLEDs =
  ColoredLEDs
    { greenLED :: Base.LED
    , orangeLED :: Base.LED
    , redLED  :: Base.LED
    , blueLED :: Base.LED
    }

data TestUART =
  TestUART
    { testUARTPeriph :: UART
    , testUARTPins   :: UARTPins
    }

data TestI2C =
  TestI2C
    { testI2C     :: I2CPeriph
    , testI2CPins :: I2CPins
    }

data TestSPI =
  TestSPI
    { testSPIPeriph :: SPIPeriph
    , testSPIPins   :: SPIPins
    }

data TestCAN =
  TestCAN
    { testCAN        :: CANPeriph
    , testCANRX      :: GPIOPin
    , testCANTX      :: GPIOPin
    , testCANFilters :: CANPeriphFilters
    }

data TestDMA =
  TestDMA
    { testDMAUARTPeriph :: DMAUART
    , testDMAUARTPins   :: UARTPins
    }

data TestPlatform =
  TestPlatform
    { testplatform_leds   :: ColoredLEDs
    , testplatform_uart1  :: TestUART
    , testplatform_uart2  :: TestUART
    , testplatform_uart3  :: TestUART
    , testplatform_i2c    :: TestI2C
    , testplatform_can1   :: TestCAN
    , testplatform_can2   :: TestCAN
    , testplatform_spi    :: TestSPI
    , testplatform_rng    :: RNG
    , testplatform_hx711  :: HX711
    , testplatform_stm32  :: STM32Config
    }

testplatform_clockconfig :: TestPlatform -> ClockConfig
testplatform_clockconfig = stm32config_clock . testplatform_stm32

-- F4DISCOVERY

f4discovery :: TestPlatform
f4discovery = TestPlatform
  { testplatform_leds = ColoredLEDs
      { greenLED = Base.LED F405.pinD12 Base.ActiveHigh
      , orangeLED = Base.LED F405.pinD13 Base.ActiveHigh
      , redLED  = Base.LED F405.pinD14 Base.ActiveHigh
      , blueLED = Base.LED F405.pinD15 Base.ActiveHigh
      }
  , testplatform_uart1 = TestUART
    { testUARTPeriph = F405.uart1
    , testUARTPins = UARTPins
          { uartPinTx = F405.pinB6
          , uartPinRx = F405.pinB7
          , uartPinAF = F405.gpio_af_uart1
          }
    }
  , testplatform_uart2 = TestUART
    { testUARTPeriph = F405.uart2
    , testUARTPins = UARTPins
        { uartPinTx = F405.pinA2
        , uartPinRx = F405.pinA3
        , uartPinAF = F405.gpio_af_uart2
        }
    }
  , testplatform_uart3 = TestUART
    { testUARTPeriph = F405.uart3
    , testUARTPins = UARTPins
        { uartPinTx = F405.pinC10
        , uartPinRx = F405.pinC11
        , uartPinAF = F405.gpio_af_uart3
        }
    }
  , testplatform_i2c = TestI2C
      { testI2C = F405.i2c2
      , testI2CPins = I2CPins
        { i2cpins_sda = F405.pinB11
        , i2cpins_scl = F405.pinB10
        }
      }
  , testplatform_can1 = TestCAN
      { testCAN = F405.can1
      , testCANRX = F405.pinB8
      , testCANTX = F405.pinB9
      , testCANFilters = F405.canFilters
      }
  , testplatform_can2 = TestCAN
      { testCAN = F405.can2
      , testCANRX = F405.pinB5
      , testCANTX = F405.pinB6
      , testCANFilters = F405.canFilters
      }
  , testplatform_spi = TestSPI
      { testSPIPeriph = F405.spi3
      , testSPIPins   = spi3_pins
      }
  , testplatform_rng = F405.rng
  , testplatform_hx711 = hx711
  , testplatform_stm32 = stm32f405Defaults 8
  }

-- slave device resets
resetPin = F405.pinD0

spi3_pins = SPIPins
  { spiPinMiso = F405.pinC12
  , spiPinMosi = F405.pinC11
  , spiPinSck  = F405.pinC10
  , spiPinAF   = F405.gpio_af_spi3
  }

max7219dev :: SPI.SPIPeriph -> SPI.SPIDevice
max7219dev spi =  SPI.SPIDevice
    { spiDevPeripheral    = spi
    , spiDevCSPin         = F405.pinD0
    , spiDevClockHz       = 500000
    , spiDevCSActive      = SPI.ActiveLow
    , spiDevClockPolarity = ClockPolarityLow
    , spiDevClockPhase    = ClockPhase1
    , spiDevBitOrder      = MSBFirst
    , spiDevName          = "max7219"
    }

max7219dev2 :: SPI.SPIPeriph -> SPI.SPIDevice
max7219dev2 spi =  SPI.SPIDevice
    { spiDevPeripheral    = spi
    , spiDevCSPin         = F405.pinD1
    , spiDevClockHz       = 500000
    , spiDevCSActive      = SPI.ActiveLow
    , spiDevClockPolarity = ClockPolarityLow
    , spiDevClockPhase    = ClockPhase1
    , spiDevBitOrder      = MSBFirst
    , spiDevName          = "max7219_2"
    }

as5407dev :: SPI.SPIPeriph -> SPI.SPIDevice
as5407dev spi =  SPI.SPIDevice
    { spiDevPeripheral    = spi
    , spiDevCSPin         = F405.pinD0
    , spiDevClockHz       = 500000
    , spiDevCSActive      = SPI.ActiveLow
    , spiDevClockPolarity = ClockPolarityLow
    , spiDevClockPhase    = ClockPhase2
    , spiDevBitOrder      = MSBFirst
    , spiDevName          = "as5407"
    }

hx711 = HX711 {
      clockPin = F405.pinE5
    , dataPin = F405.pinE6
    }
