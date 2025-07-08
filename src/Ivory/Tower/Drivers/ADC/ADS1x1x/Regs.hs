{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.Tower.Drivers.ADC.ADS1x1x.Regs where

import Ivory.Language
import Ivory.Tower.Drivers.ADC.ADS1x1x.RegTypes

[ivory|
 bitdata POINTER :: Bits 8 = ads_pointer
   { _      :: Bits 6
   , adsReg :: AdsReg
   }

 bitdata CONVERSION :: Bits 16 = ads_conversion
   { conversion :: Bits 12
   , _          :: Bits 4
   }

 bitdata CONFIG :: Bits 16 = ads_config
   { config_os        :: OperationalStatus
   , config_mux       :: MUX
   , config_pga       :: PGA
   , config_mode      :: Mode
   , config_datarate  :: DataRate
   , config_comp_mode :: ComparatorMode
   , config_comp_pol  :: ComparatorPolarity
   , config_comp_lat  :: ComparatorLatching
   , config_comp_que  :: ComparatorQueue
   }

 bitdata THRESH :: Bits 16 = ads_thresh
   { threshold :: Bits 12
   , _         :: Bits 4
   }
|]
