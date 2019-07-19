{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.Tower.Drivers.Temperature.HTS221.Regs where

import Ivory.Language
import Ivory.Tower.Drivers.Temperature.HTS221.RegTypes

[ivory|
 bitdata AVCONF :: Bits 8 = hts_avconf_reg
   { _       :: Bits 2
   , avgTemp :: AVGT
   , avgHumi :: AVGH
   }

 bitdata CTRL1 :: Bits 8 = hts_ctrl1_reg
   { power           :: Power
   , _               :: Bits 4
   , blockDataUpdate :: BDU
   , outputDataRate  :: Rate
   }

 bitdata CTRL2 :: Bits 8 = hts_ctrl2_reg
   { rebootMemory :: Bit
   , _            :: Bits 5
   , heater       :: Bit
   , oneShot      :: Bit
   }

 bitdata CTRL3 :: Bits 8 = hts_ctrl3_reg
   { drdyHighLow :: DRDY
   , ppOd        :: PPOD -- push pull / open drain
   , _           :: Bits 3
   , drdyEnable  :: Bit
   , _           :: Bits 2
   }

 bitdata STATUS :: Bits 8 = hts_status_reg
   { _             :: Bits 6
   , humiAvailable :: Bit
   , tempAvailable :: Bit
   }

|]
