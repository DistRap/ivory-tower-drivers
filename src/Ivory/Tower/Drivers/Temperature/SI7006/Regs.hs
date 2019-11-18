{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Ivory.Tower.Drivers.Temperature.SI7006.Regs where

import Ivory.Language
import Ivory.Tower.Drivers.Temperature.SI7006.RegTypes

-- heaterCurrent is 3.09 mA @ 0b0000
--             and 94.20 mA @ 0b1111
--
-- default resolution (res0=0, res1=0) is 12b Rel.Humidity, 14b Temperature

[ivory|
 bitdata USER :: Bits 8 = si_user_reg
   { res1       :: Bit
   , vdds       :: VDDS
   , _          :: Bits 3
   , heater     :: HEATER
   , _          :: Bit
   , res0       :: Bit
   }

 bitdata HEAT :: Bits 8 = si_heater_reg
   { _               :: Bits 4
   , heaterCurrent   :: Bits 4
   }
|]
