module Ivory.Tower.Drivers.SPIDevice where

import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.Peripheral.SPI

-- | Default SPI device helper
-- CPOL=0 CPHA=0
genericSPIDev :: String -> SPI -> GPIOPin -> SPIDevice
genericSPIDev name spi csPin =  SPIDevice
    { spiDevPeripheral    = spi
    , spiDevCSPin         = csPin
    , spiDevClockHz       = 500000
    , spiDevCSActive      = ActiveLow
    , spiDevClockPolarity = ClockPolarityLow
    , spiDevClockPhase    = ClockPhase1
    , spiDevBitOrder      = MSBFirst
    , spiDevName          = name ++ pinName csPin
    }

-- | Alias to `ClockPolarityLow`
cpol0 :: SPIClockPolarity
cpol0 = ClockPolarityLow

-- | Alias to `ClockPolarityHigh`
cpol1 :: SPIClockPolarity
cpol1 = ClockPolarityHigh

-- | Alias to `ClockPhase1`
cpha0 :: SPIClockPhase
cpha0 = ClockPhase1

-- | Alias to `ClockPhase2`
cpha1 :: SPIClockPhase
cpha1 = ClockPhase2
