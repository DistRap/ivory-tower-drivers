{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Drivers.Encoder.AS5047.Regs where

import Ivory.Language

[ivory|
 bitdata AS5047Cmd :: Bits 16 = as5047cmd
   { as_parc   :: Bit
   , as_read   :: Bit
   , as_addr   :: Bits 14 }

 bitdata AS5047Data :: Bits 16 = as5047data
   { as_pard   :: Bit
   , as_error  :: Bit
   , as_data   :: Bits 14 }

  bitdata ERRFL :: Bits 14 = as5047errfl
  { _              :: Bits 11
  , as_err_parerr  :: Bit
  , as_err_invcomm :: Bit
  , as_err_frerr   :: Bit
  }

  bitdata PROG :: Bits 14 = as5047prog
  { _                :: Bits 7
  , as_prog_progver  :: Bit
  , _                :: Bits 2
  , as_prog_progotp  :: Bit
  , as_prog_otpref   :: Bit
  , _                :: Bit
  , as_prog_progen   :: Bit
  }

  bitdata DIAGAGC :: Bits 14 = as5047diag
  { _                :: Bits 2
  , as_diag_magl     :: Bit
  , as_diag_magh     :: Bit
  , as_diag_cof      :: Bit
  , as_diag_lf       :: Bit
  , as_diag_agc      :: Bits 8
  }

  bitdata MAG :: Bits 14 = as5047mag
  { as_mag_data      :: Bits 14
  }

  bitdata CORDICANG :: Bits 14 = as5047cordic
  { as_cordic_data   :: Bits 14
  }

  bitdata DAECANG :: Bits 14 = as5047daecang
  { as_dae_data      :: Bits 14
  }

  bitdata ZPOSL :: Bits 8 = as5047zposl
  { as_zposl_comp_h_error_en :: Bit
  , as_zposl_comp_l_error_en :: Bit
  , as_zposl_lsb             :: Bits 6
  }

  bitdata SETTINGS1 :: Bits 8 = as5047settings1
  { as_settings1_pwm_on       :: Bit
  , as_settings1_dataselect   :: Bit
  , as_settings1_abi_bin      :: Bit
  , as_settings1_daec_disable :: Bit
  , as_settings1_uvw_abi      :: Bit
  , as_settings1_dir          :: Bit
  , as_settings1_noiseset     :: Bit
  , _                         :: Bit
  }

  bitdata SETTINGS2 :: Bits 8 = as5047settings2
  { as_settings2_abi_resolution :: Bits 3
  , as_settings2_hysteresis     :: Bits 2
  , as_settings2_uwv_pole_pairs :: Bits 3
  }
|]
