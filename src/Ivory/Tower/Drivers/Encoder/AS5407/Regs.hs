{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Drivers.Encoder.AS5407.Regs where

import Ivory.Language

[ivory|
 bitdata AS5407Cmd :: Bits 16 = as5407cmd
   { as_parc   :: Bit
   , as_read   :: Bit
   , as_addr   :: Bits 14 }

 bitdata AS5407Data :: Bits 16 = as5407data
   { as_pard   :: Bit
   , as_error  :: Bit
   , as_data   :: Bits 14 }

  bitdata ERRFL :: Bits 14 = as5407errfl
  { _              :: Bits 11
  , as_err_parerr  :: Bit
  , as_err_invcomm :: Bit
  , as_err_frerr   :: Bit
  }

  bitdata PROG :: Bits 14 = as5407prog
  { _                :: Bits 7
  , as_prog_progver  :: Bit
  , _                :: Bits 2
  , as_prog_progotp  :: Bit
  , as_prog_otpref   :: Bit
  , _                :: Bit
  , as_prog_progen   :: Bit
  }

  bitdata DIAGAGC :: Bits 14 = as5407diag
  { _                :: Bits 2
  , as_diag_magl     :: Bit
  , as_diag_magh     :: Bit
  , as_diag_cof      :: Bit
  , as_diag_lf       :: Bit
  , as_diag_agc      :: Bits 8
  }

  bitdata MAG :: Bits 14 = as5407mag
  { as_mag_data      :: Bits 14
  }

  bitdata CORDICANG :: Bits 14 = as5407cordic
  { as_cordic_data   :: Bits 14
  }

  bitdata DAECANG :: Bits 14 = as5407daecang
  { as_dae_data      :: Bits 14
  }
|]
