{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Ivory.Tower.Drivers.Net.RS485
  ( rs485Tower
  ) where

import Ivory.HW (hw_moduledef)
import Ivory.Language
import Ivory.Tower
import Ivory.Stdlib ((==>), cond_)
import Ivory.Tower.HAL.Bus.Interface (BackpressureTransmit(..))
import Ivory.BSP.STM32.ClockConfig (ClockConfig)
import Ivory.BSP.STM32.Driver.UART
import Ivory.BSP.STM32.Peripheral.GPIO
import Ivory.BSP.STM32.Peripheral.UART (UART, UARTPins)

-- | Half-duplex RS485 driver
-- Switches dir output pin 1ms after transmission
-- occurs.
--
-- This is not exactly according to RS485 spec
-- (switch should occur faster) but if there's no delay
-- and we switch dir right after @BackpressureTransmit@s complete
-- message, we cut out last byte (since the interrupt used
-- for complete channel is not TC - transfer complete).
rs485Tower
  :: forall s e
   . IvoryString s
  => (e -> ClockConfig) -- ^ ClockConfig
  -> UART -- ^ UART peripheral
  -> UARTPins -- ^ UART Pins
  -> GPIOPin -- ^ RS485 dir pin
  -> Integer -- ^ Baudrate
  -> Tower e
       ( ChanInput s
       , ChanOutput (Stored Uint8)
       )
rs485Tower tocc uartPeriph uartPins dirPin baud = do
  (BackpressureTransmit req res, istream, rsMon)
    <- uartTower
        @s
        tocc
        uartPeriph
        uartPins
        baud
  monitor (named "UartMonitor") rsMon

  req485 <- channel
  istream485 <- channel

  dirPeriod <- period (Milliseconds 1)

  monitor (named "Monitor") $ do
    monitorModuleDef $ hw_moduledef
    handler systemInit (named "Init") $ do
      callback $ const $ do
        pinEnable dirPin
        pinSetMode dirPin gpio_mode_output

    handler (snd req485) (named "Request") $ do
      reqE <- emitter req 1
      callback $ \msg -> do
        -- enable driver
        pinSet dirPin
        emit reqE msg

    dirDelay
      <- stateInit
           (named "DirDelay")
           (ival (0 :: Uint8))

    handler res (named "DoneSending") $ do
      callback $ const $ store dirDelay 2

    handler dirPeriod (named "DirPeriod") $ do
      callback $ const $ do
        delay <- deref dirDelay
        cond_
          [ delay ==? 1 ==> do
              -- disable driver
              pinClear dirPin
              store dirDelay 0
          , delay >? 1 ==>
              store dirDelay (delay - 1)
          ]

    handler istream (named "Istream") $ do
      o <- emitter (fst istream485) 1
      callback $ emit o

  pure
    ( fst req485
    , snd istream485
    )
  where
    named x = "rs485" <> x
