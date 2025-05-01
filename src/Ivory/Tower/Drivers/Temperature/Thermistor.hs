{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Ivory.Tower.Drivers.Temperature.Thermistor where

import Ivory.Language
import Ivory.Tower

data Thermistor = Thermistor {
    t1       :: IFloat -- low temperature (around 25°C)
  , r1       :: IFloat -- resistance at low temperature point

  , t2       :: IFloat -- mid temperature (around 150°C)
  , r2       :: IFloat -- resistance at mid temperature point

  , t3       :: IFloat -- high temperature (around 250°C)
  , r3       :: IFloat -- resistance at high temperature point

  , dividerR :: IFloat -- divider resistor resistance
  , adcV     :: IFloat -- ADC reference voltage
  , adcPrec  :: Uint8  -- ADC precision in bits (typically 12bits)
}

absoluteZero :: IFloat
absoluteZero = 273.15

-- | Compute temperature from raw ADC values
-- according to Steinhart-Hart equation
-- https://en.wikipedia.org/wiki/Steinhart-Hart_equation
-- using parameters from @Thermistor@
thermistorTower
  :: (ChanOutput ('Stored Uint16))
  -> Thermistor
  -> Tower e (ChanOutput ('Stored IFloat))
thermistorTower adcIn Thermistor{..} = do

  (convertedIn, convertedOut) <- channel

  monitor "thermistor" $ do
    -- computed Steinhart–Hart coefficients
    sh_a <- state "sh_a"
    sh_b <- state "sh_b"
    sh_c <- state "sh_c"

    handler systemInit "thermistor_init" $ do
      callback $ const $ do
        let l1 = log r1
            l2 = log r2
            l3 = log r3

            y1 = 1 / (t1 + absoluteZero)
            y2 = 1 / (t2 + absoluteZero)
            y3 = 1 / (t3 + absoluteZero)

            x = (y2 - y1) / (l2 - l1)
            y = (y3 - y1) / (l3 - l1)

            c = (y - x) / ((l3 - l2) * (l1 + l2 + l3))
            b = x - c * (l1 ** 2 + l1 * l2 + l2 ** 2 )
            a = y1 - (b + l1 ** 2 * c) * l1

        store sh_c c
        store sh_b b
        store sh_a a

    volt <- state "volt"
    resi <- state "resi"

    handler adcIn "thermistor_adc_in" $ do
      cvIn <- emitter convertedIn 1
      callbackV $ \raw -> do

        voltage <- assign $ (safeCast raw) * (adcV / (2 ** safeCast adcPrec))
        resistance <- assign $ dividerR * voltage / (adcV - voltage)
        lnr <- assign $ log resistance

        store volt voltage
        store resi resistance

        a <- deref sh_a
        b <- deref sh_b
        c <- deref sh_c

        inverted <- assign $ a + b * lnr + c * lnr ** 3

        emitV cvIn $ 1/inverted - absoluteZero

  return convertedOut

sampleTerm = Thermistor {
    t1 = 25.0
  , r1 = 100000 -- 100kOhm

  , t2 = 150
  , r2 = 1770

  , t3 = 250
  , r3 = 230

  , dividerR = 4700 -- 4.7kOhm
  , adcV = 3.3
  , adcPrec = 12
  }
