{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Ivory.Tower.Drivers.Display.Fonts where

import Ivory.Language
import Ivory.Stdlib

-- 7 segment font for MAX7219
--
--    -- A --
--   |       |
--   F       B
--   |       |
--    -- G --
--   |       |
--   E       C
--   |       |
--    -- D --  (DP)
--
--
-- Register bit | 7  6  5  4  3  2  1  0
-- Segment      | DP A  B  C  D  E  F  G


import Data.Char

font7seg :: Char -> Uint8
font7seg ' ' = 0x00
font7seg '-' = 0x01
font7seg '_' = 0x08
font7seg '0' = 0x7e
font7seg '1' = 0x30
font7seg '2' = 0x6d
font7seg '3' = 0x79
font7seg '4' = 0x33
font7seg '5' = 0x5b
font7seg '6' = 0x5f
font7seg '7' = 0x70
font7seg '8' = 0x7f
font7seg '9' = 0x7b
font7seg 'a' = 0x7d
font7seg 'b' = 0x1f
font7seg 'c' = 0x0d
font7seg 'd' = 0x3d
font7seg 'e' = 0x6f
font7seg 'f' = 0x47
font7seg 'g' = 0x7b
font7seg 'h' = 0x17
font7seg 'i' = 0x10
font7seg 'j' = 0x18
--font7seg 'k' = 0x08 -- not supported
font7seg 'l' = 0x06
--font7seg 'm' = 0x08 -- not supported
font7seg 'n' = 0x15
font7seg 'o' = 0x1d
font7seg 'p' = 0x67
font7seg 'q' = 0x73
font7seg 'r' = 0x05
font7seg 's' = 0x5b
font7seg 't' = 0x0f
font7seg 'u' = 0x1c
font7seg 'v' = 0x1c
--font7seg 'w' = 0x08 -- not supported
--font7seg 'x' = 0x08 -- not supported
font7seg 'y' = 0x3b
font7seg 'z' = 0x6d
font7seg 'A' = 0x77
font7seg 'B' = 0x7f
font7seg 'C' = 0x4e
font7seg 'D' = 0x7e
font7seg 'E' = 0x4f
font7seg 'F' = 0x47
font7seg 'G' = 0x5e
font7seg 'H' = 0x37
font7seg 'I' = 0x30
font7seg 'J' = 0x38
--font7seg 'K' = 0x08 -- not supported
font7seg 'L' = 0x0e
--font7seg 'M' = 0x08 -- not supported
font7seg 'N' = 0x76
font7seg 'O' = 0x7e
font7seg 'P' = 0x67
font7seg 'Q' = 0x73
font7seg 'R' = 0x46
font7seg 'S' = 0x5b
font7seg 'T' = 0x0f
font7seg 'U' = 0x3e
font7seg 'V' = 0x3e
--font7seg 'W' = 0x08 -- not supported
--font7seg 'X' = 0x08 -- not supported
font7seg 'Y' = 0x3b
font7seg 'Z' = 0x6d
font7seg ',' = 0x80
font7seg '.' = 0x80
font7seg '°' = 0x63
font7seg  _   = 0x00

--font7seg' :: (GetAlloc eff ~ 'Scope s) => Uint8 -> Ivory eff ('Stored Uint8)
font7seg' x = cond [
    x ==?  32 ==> return 0x00
  , x ==?  45 ==> return 0x01
  , x ==?  95 ==> return 0x08
  , x ==?  48 ==> return 0x7e
  , x ==?  49 ==> return 0x30
  , x ==?  50 ==> return 0x6d
  , x ==?  51 ==> return 0x79
  , x ==?  52 ==> return 0x33
  , x ==?  53 ==> return 0x5b
  , x ==?  54 ==> return 0x5f
  , x ==?  55 ==> return 0x70
  , x ==?  56 ==> return 0x7f
  , x ==?  57 ==> return 0x7b
  , x ==?  97 ==> return 0x7d
  , x ==?  98 ==> return 0x1f
  , x ==?  99 ==> return 0x0d
  , x ==? 100 ==> return 0x3d
  , x ==? 101 ==> return 0x6f
  , x ==? 102 ==> return 0x47
  , x ==? 103 ==> return 0x7b
  , x ==? 104 ==> return 0x17
  , x ==? 105 ==> return 0x10
  , x ==? 106 ==> return 0x18
  , x ==? 107 ==> return 0x08
  , x ==? 108 ==> return 0x06
  , x ==? 109 ==> return 0x08
  , x ==? 110 ==> return 0x15
  , x ==? 111 ==> return 0x1d
  , x ==? 112 ==> return 0x67
  , x ==? 113 ==> return 0x73
  , x ==? 114 ==> return 0x05
  , x ==? 115 ==> return 0x5b
  , x ==? 116 ==> return 0x0f
  , x ==? 117 ==> return 0x1c
  , x ==? 118 ==> return 0x1c
  , x ==? 119 ==> return 0x08
  , x ==? 120 ==> return 0x08
  , x ==? 121 ==> return 0x3b
  , x ==? 122 ==> return 0x6d
  , x ==?  65 ==> return 0x77
  , x ==?  66 ==> return 0x7f
  , x ==?  67 ==> return 0x4e
  , x ==?  68 ==> return 0x7e
  , x ==?  69 ==> return 0x4f
  , x ==?  70 ==> return 0x47
  , x ==?  71 ==> return 0x5e
  , x ==?  72 ==> return 0x37
  , x ==?  73 ==> return 0x30
  , x ==?  74 ==> return 0x38
  , x ==?  75 ==> return 0x08
  , x ==?  76 ==> return 0x0e
  , x ==?  77 ==> return 0x08
  , x ==?  78 ==> return 0x76
  , x ==?  79 ==> return 0x7e
  , x ==?  80 ==> return 0x67
  , x ==?  81 ==> return 0x73
  , x ==?  82 ==> return 0x46
  , x ==?  83 ==> return 0x5b
  , x ==?  84 ==> return 0x0f
  , x ==?  85 ==> return 0x3e
  , x ==?  86 ==> return 0x3e
  , x ==?  87 ==> return 0x08
  , x ==?  88 ==> return 0x08
  , x ==?  89 ==> return 0x3b
  , x ==?  90 ==> return 0x6d
  , x ==?  44 ==> return 0x80
  , x ==?  46 ==> return 0x80
  , x ==? 176 ==> return 0x63
  , true      ==> return 0x00
  ]
