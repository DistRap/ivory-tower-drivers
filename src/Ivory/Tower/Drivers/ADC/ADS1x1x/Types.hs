module Ivory.Tower.Drivers.ADC.ADS1x1x.Types where

import Data.List.NonEmpty
import Ivory.Language
import Ivory.Tower.Drivers.ADC.ADS1x1x.RegTypes

-- | Sampling mode
data ADSMode
  = ADSMode_Continuous ADSMux
  -- ^ Continuosly sample a single channel/mux
  | ADSMode_Single (NonEmpty ADSMux)
  -- ^ Sample a sequence of muxes (1 up to 4 muxes)
  deriving (Eq, Ord, Show)

adsModeVal :: ADSMode -> Mode
adsModeVal (ADSMode_Continuous _) = modeContinuous
adsModeVal (ADSMode_Single _)     = modeSingle

-- | MUX Configuration (4 channel variants only)
data ADSMux
  = ADSMux_Differential_0_1
  | ADSMux_Differential_0_3
  | ADSMux_Differential_1_3
  | ADSMux_Differential_2_3
  -- Single-ended
  | ADSMux_Single_0
  | ADSMux_Single_1
  | ADSMux_Single_2
  | ADSMux_Single_3
  deriving (Eq, Ord, Show)

adsMuxVal :: ADSMux -> MUX
adsMuxVal ADSMux_Differential_0_1 = muxDiff_0_1
adsMuxVal ADSMux_Differential_0_3 = muxDiff_0_3
adsMuxVal ADSMux_Differential_1_3 = muxDiff_1_3
adsMuxVal ADSMux_Differential_2_3 = muxDiff_2_3
adsMuxVal ADSMux_Single_0         = muxSingle_0
adsMuxVal ADSMux_Single_1         = muxSingle_1
adsMuxVal ADSMux_Single_2         = muxSingle_2
adsMuxVal ADSMux_Single_3         = muxSingle_3

-- | Full scale range of programmable gain amplifier (PGA)
-- always +-V(olts)
data ADSPGA
  = ADSPGA_6point144volts
  | ADSPGA_4point096volts
  | ADSPGA_2point048volts -- ^ Default
  | ADSPGA_1point024volts
  | ADSPGA_512millivolts
  | ADSPGA_256millivolts
  deriving (Eq, Ord, Show)

adsPGAVal :: ADSPGA -> PGA
adsPGAVal ADSPGA_6point144volts = pga_fsr_6_144
adsPGAVal ADSPGA_4point096volts = pga_fsr_4_096
adsPGAVal ADSPGA_2point048volts = pga_fsr_2_048
adsPGAVal ADSPGA_1point024volts = pga_fsr_1_024
adsPGAVal ADSPGA_512millivolts  = pga_fsr_0_512
adsPGAVal ADSPGA_256millivolts  = pga_fsr_0_256

adsPGAToIFloat :: ADSPGA -> IFloat
adsPGAToIFloat ADSPGA_6point144volts = 6.144
adsPGAToIFloat ADSPGA_4point096volts = 4.096
adsPGAToIFloat ADSPGA_2point048volts = 2.048
adsPGAToIFloat ADSPGA_1point024volts = 1.024
adsPGAToIFloat ADSPGA_512millivolts  = 0.512
adsPGAToIFloat ADSPGA_256millivolts  = 0.256

-- | Data rate for ADS101x family
data ADS101xDataRate
   = ADS101xDataRate_128SPS
   | ADS101xDataRate_250SPS
   | ADS101xDataRate_490SPS
   | ADS101xDataRate_920SPS
   | ADS101xDataRate_1600SPS -- ^ Default
   | ADS101xDataRate_2400SPS
   | ADS101xDataRate_3300SPS
   deriving (Eq, Ord, Show)

ads101xDataRateVal :: ADS101xDataRate -> DataRate
ads101xDataRateVal ADS101xDataRate_128SPS  = dr_ads101x_128
ads101xDataRateVal ADS101xDataRate_250SPS  = dr_ads101x_250
ads101xDataRateVal ADS101xDataRate_490SPS  = dr_ads101x_490
ads101xDataRateVal ADS101xDataRate_920SPS  = dr_ads101x_920
ads101xDataRateVal ADS101xDataRate_1600SPS = dr_ads101x_1600
ads101xDataRateVal ADS101xDataRate_2400SPS = dr_ads101x_2400
ads101xDataRateVal ADS101xDataRate_3300SPS = dr_ads101x_3300

-- | Data rate for ADS111x family
data ADS111xDataRate
   = ADS111xDataRate_8SPS
   | ADS111xDataRate_16SPS
   | ADS111xDataRate_32SPS
   | ADS111xDataRate_64SPS
   | ADS111xDataRate_128SPS -- ^ Default
   | ADS111xDataRate_250SPS
   | ADS111xDataRate_475SPS
   | ADS111xDataRate_860SPS
   deriving (Eq, Ord, Show)

ads111xDataRateVal :: ADS111xDataRate -> DataRate
ads111xDataRateVal ADS111xDataRate_8SPS   = dr_ads111x_8
ads111xDataRateVal ADS111xDataRate_16SPS  = dr_ads111x_16
ads111xDataRateVal ADS111xDataRate_32SPS  = dr_ads111x_32
ads111xDataRateVal ADS111xDataRate_64SPS  = dr_ads111x_64
ads111xDataRateVal ADS111xDataRate_128SPS = dr_ads111x_128
ads111xDataRateVal ADS111xDataRate_250SPS = dr_ads111x_250
ads111xDataRateVal ADS111xDataRate_475SPS = dr_ads111x_475
ads111xDataRateVal ADS111xDataRate_860SPS = dr_ads111x_860

-- | Data rate for either ADS101x or ADS111x
data ADSDataRate
  = ADSDataRate_ADS101x ADS101xDataRate
  | ADSDataRate_ADS111x ADS111xDataRate
  deriving (Eq, Ord, Show)

adsDataRateVal :: ADSDataRate -> DataRate
adsDataRateVal (ADSDataRate_ADS101x dr) = ads101xDataRateVal dr
adsDataRateVal (ADSDataRate_ADS111x dr) = ads111xDataRateVal dr

-- | ADS1x1x ADC driver configuration
data ADSConfig = ADSConfig
  { adsConfigPGA      :: ADSPGA          -- ^ Progammable gain amplifier
  , adsConfigDataRate :: ADSDataRate     -- ^ Data rate
  , adsConfigMode     :: ADSMode         -- ^ Continuous or single conversion mode
  }
  deriving (Eq, Ord, Show)

-- | Default sampling sequence (all channels in single-ended mode)
defaultSequence :: NonEmpty ADSMux
defaultSequence =
  ADSMux_Single_0
  :|
  [ ADSMux_Single_1
  , ADSMux_Single_2
  , ADSMux_Single_3
  ]

-- | ADS111x configuration for sampling all channels
-- in single-ended mode at the highest rate
configADS111xAllChannels :: ADSConfig
configADS111xAllChannels = ADSConfig
  { adsConfigPGA      = ADSPGA_2point048volts
  , adsConfigDataRate = ADSDataRate_ADS111x
                          ADS111xDataRate_860SPS
  , adsConfigMode     = ADSMode_Single
                          defaultSequence
  }

-- | ADS111x configuration for continuously sampling
-- channel 0
configADS111xChannel0 :: ADSConfig
configADS111xChannel0 = configADS111xAllChannels
  { adsConfigMode     = ADSMode_Continuous
                          ADSMux_Single_0
  }

-- | ADS101x configuration for sampling all channels
-- in single-ended mode at the highest rate
configADS101xAllChannels :: ADSConfig
configADS101xAllChannels = configADS111xAllChannels
  { adsConfigDataRate = ADSDataRate_ADS101x
                          ADS101xDataRate_3300SPS
  }

-- | ADS101x configuration for continuously sampling
-- channel 0
configADS101xChannel0 :: ADSConfig
configADS101xChannel0 = configADS101xAllChannels
  { adsConfigMode     = ADSMode_Continuous
                          ADSMux_Single_0
  }
