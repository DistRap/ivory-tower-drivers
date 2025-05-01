{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Drivers.DAC.DACx1408.Regs where

import Ivory.Language

import Ivory.Tower.Drivers.DAC.DACx1408.RegTypes

[ivory|
  bitdata DeviceID :: Bits 16 = dacRegDeviceID
    { device_id_data    :: Bits 14
    , device_id_version :: Bits 2
    }

  bitdata Status :: Bits 16 = dacRegStatus
    { _               :: Bits 13
    , status_crc_fail :: Bit -- ^ CRC error
    , status_dac_busy :: Bit -- ^ DAC registers not ready for updates
    , status_overtemp :: Bit -- ^ Over temperature alarm
    }

  bitdata SPI :: Bits 16 = dacRegSPI
    { _                         :: Bits 4
    , spi_temp_alarm_enable     :: Bit -- ^ When set to 1 a thermal alarm triggers the ^ALMOUT pin
    , spi_dac_busy_alarm_enable :: Bit -- ^ When set to 1 the ^ALMOUT pin is set between DAC output updates
    , spi_crc_alarm_enable      :: Bit -- ^ When set to 1 the ^ALMOUT pin is set on CRC error
    , _                         :: Bits 2
    , spi_soft_toggle_enable    :: Bit -- ^ Enable soft toggle operation
    , spi_powerdown             :: Bit -- ^ Set to power down the device
    , spi_crc_enable            :: Bit -- ^ Enable CRC error checking
    , spi_streaming_enable      :: Bit -- ^ Enable streaming operation
    , spi_sdo_enable            :: Bit -- ^ Enable SDO pin
    , spi_fast_sdo_enable       :: Bit -- ^ Fast SDO bit (half-cycle speedup). When 0, SDO updates during
                                       -- SCLK rising edges. When 1, SDO updates during SCLK falling edges
    , _                         :: Bit
    }

  bitdata General :: Bits 16 = dacRegGeneral
    { _                             :: Bit
    , general_ref_powerdown         :: Bit -- ^ Powers down the internal reference
    , _                             :: Bits 8
    , general_dac6and7_differential :: Bit -- ^ Differential signaling on this pair
    , general_dac4and5_differential :: Bit
    , general_dac2and3_differential :: Bit
    , general_dac0and1_differential :: Bit
    , _                             :: Bits 2
    }

  bitdata BroadcastCfg :: Bits 16 = dacRegBroadcastCfg
    { _                         :: Bits 4
    , broadcast_cfg_dac7_enable :: Bit
    , broadcast_cfg_dac6_enable :: Bit
    , broadcast_cfg_dac5_enable :: Bit
    , broadcast_cfg_dac4_enable :: Bit
    , broadcast_cfg_dac3_enable :: Bit
    , broadcast_cfg_dac2_enable :: Bit
    , broadcast_cfg_dac1_enable :: Bit
    , broadcast_cfg_dac0_enable :: Bit
    , _                         :: Bits 4
    }

  bitdata Sync :: Bits 16 = dacRegSync
    { _                :: Bits 4
    , sync_dac7_enable :: Bit
    , sync_dac6_enable :: Bit
    , sync_dac5_enable :: Bit
    , sync_dac4_enable :: Bit
    , sync_dac3_enable :: Bit
    , sync_dac2_enable :: Bit
    , sync_dac1_enable :: Bit
    , sync_dac0_enable :: Bit
    , _                :: Bits 4
    }

  bitdata Toggle0 :: Bits 16 = dacRegToggle0
    { _                :: Bits 8
    , toggle_dac7_mode :: ToggleMode
    , toggle_dac6_mode :: ToggleMode
    , toggle_dac5_mode :: ToggleMode
    , toggle_dac4_mode :: ToggleMode
    }

  bitdata Toggle1 :: Bits 16 = dacRegToggle1
    { toggle_dac3_mode :: ToggleMode
    , toggle_dac2_mode :: ToggleMode
    , toggle_dac1_mode :: ToggleMode
    , toggle_dac0_mode :: ToggleMode
    , _                :: Bits 8
    }

  bitdata PowerDown :: Bits 16 = dacRegPowerDown
    { _                     :: Bits 4
    , powerdown_dac7_enable :: Bit
    , powerdown_dac6_enable :: Bit
    , powerdown_dac5_enable :: Bit
    , powerdown_dac4_enable :: Bit
    , powerdown_dac3_enable :: Bit
    , powerdown_dac2_enable :: Bit
    , powerdown_dac1_enable :: Bit
    , powerdown_dac0_enable :: Bit
    , _                     :: Bits 4
    }

  bitdata Range :: Bits 16 = dacRegRange
    { range_dac_a :: RangeVolts
    , range_dac_b :: RangeVolts
    , range_dac_c :: RangeVolts
    , range_dac_d :: RangeVolts
    }

  bitdata Trigger :: Bits 16 = dacRegTrigger
    { _                   :: Bits 7
    , trigger_alarm_reset :: Bit       -- ^ Clear alarm event
    , trigger_ab_toggle2  :: Bit       -- ^ Set to update register B
    , trigger_ab_toggle1  :: Bit       -- ^ Set to update register B
    , trigger_ab_toggle0  :: Bit       -- ^ Set to update register B
    , trigger_ldac        :: Bit       -- ^ Synchronously load DACs in synchronous mode
    , trigger_soft_reset  :: SoftReset -- ^ Soft reset trigger
    }

  bitdata Offset :: Bits 16 = dacRegOffset
    { offset_ab :: Bits 8
    , offset_cd :: Bits 8
    }
|]
