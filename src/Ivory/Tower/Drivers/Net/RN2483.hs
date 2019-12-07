{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Ivory.Tower.Drivers.Net.RN2483 (
    rn2483Tower
  , module Ivory.Tower.Drivers.Net.RN2483.Types
  , module Ivory.Tower.Drivers.Net.RN2483.Config
  ) where

import Control.Monad (forM_)

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.Base.UART

import Ivory.HW.Module

import Ivory.BSP.STM32.Peripheral.GPIO

import Ivory.Tower.Drivers.Net.RN2483.Types
import Ivory.Tower.Drivers.Net.RN2483.Config

named :: String -> String
named n = "rn2483" ++ n

rn2483Tower :: forall buf e . (IvoryString buf)
            => RadioConfig
            -> ChanInput ('Stored Uint8)
            -> ChanOutput ('Stored Uint8)
            -> ChanOutput ('Stored ITime)
            -> GPIOPin
            -> Proxy buf
            -> Tower e ( ChanOutput ('Stored IBool)
                       , ChanInput ('Stored RadioCommandMode)
                       , BackpressureTransmit buf ('Stored IBool))
rn2483Tower cfg ostream istreamChar initChan rstPin proxybuf = do
  checkRadioConfig cfg

  acceptChan  <- channel
  txdoneChan  <- channel
  cmdChan     <- channel
  cmdModeChan <- channel

  rejoinPer <- period (Milliseconds 8000)

  istream <- crlfBuffer istreamChar proxybuf

  monitor (named "mon") $ do
    retryJoin   <- state     (named "retryJoin")
    cmdMode     <- stateInit (named "commandMode") (ival modeUnconfirmed)
    transmitted <- stateInit (named "transmitted") (ival (0 :: Uint32))
    commandsOk  <- stateInit (named "oks")         (ival (0 :: Uint32))
    commandsErr <- stateInit (named "errors")      (ival (0 :: Uint32))

    -- due to pins
    monitorModuleDef $ hw_moduledef

    handler systemInit "rn2483init" $ do
      callback $ const $ do
        pinEnable rstPin
        pinSetMode rstPin gpio_mode_output
        pinClear rstPin

    handler rejoinPer "rn2483per" $ do
      o <- emitter ostream 64
      callback $ const $ do
        go <- deref retryJoin
        when go $ do
          puts o $ "mac join " ++ (rnActivationMode $ rcActivationMode cfg) ++ "\r\n"

    handler (snd cmdModeChan) "rn2483cmdMode" $ callback $ refCopy cmdMode

    handler (snd cmdChan) "rn2483cmd" $ do
      o <- emitter ostream 64
      callback $ \cmd -> do
        mode <- deref cmdMode
        cond_ [
            mode ==? modeUnconfirmed ==> do
              puts o "mac tx uncnf 1 "
              putHexIvoryString o cmd
          , mode ==? modeConfirmed   ==> do
              puts o "mac tx cnf 1 "
              putHexIvoryString o cmd
          , mode ==? modeRaw         ==>
              putIvoryString o cmd
          ]
        puts o "\r\n"

    coroutineHandler initChan istream "rn2483i" $ do
      o <- emitter ostream 64
      accept <- emitter (fst acceptChan) 1
      txd <- emitter (fst txdoneChan) 1
      return $ CoroutineBody $ \ yield -> do

        let isPrefixOf p x = noBreak $ isPrefixOfBuf p x (Proxy :: Proxy buf)
            sendCmd cmd = puts o $ cmd ++ "\r\n"
            rpc cmd = do
               sendCmd cmd
               res <- yield
               gotOk <- "ok" `isPrefixOf` res
               cond_ [
                   gotOk ==> commandsOk += 1
                 , true  ==> commandsErr += 1
                 ]
               return ()

        pinSet rstPin

        input <- yield
        res <- "RN2483" `isPrefixOf` input
        when res $ do
          forM_ (rn2483Configure cfg) $ \cmd -> rpc cmd

          sendCmd $ "mac join " ++ (rnActivationMode $ rcActivationMode cfg)
          joinRes <- yield
          noKeys <- "keys_not_init" `isPrefixOf` joinRes

          when noKeys $ do
            case rcActivation cfg of
              Nothing -> return ()
              Just act -> do
                forM_ (rnActivate act) rpc

                -- rpc expects/eats 'ok'
                rpc $ "mac join " ++ (rnActivationMode $ rcActivationMode cfg)

          forever $ do
            joinres <- yield

            accepted <- noBreak $ "accept" `isPrefixOf` joinres
            denied <-   noBreak $ "denied" `isPrefixOf` joinres

            cond_
              [ accepted ==> do
                  store retryJoin false
                  emitV accept true
                  breakOut
              , denied ==> do
                  store retryJoin true
                  _ <- yield
                  return ()
              ]

          forever $ noBreak $ do
            -- msg from handler above
            resp <- yield
            gotOk <- "ok" `isPrefixOf` resp
            txOk  <- "mac_tx_ok" `isPrefixOf` resp

            cond_ [
                gotOk ==> commandsOk += 1
              , txOk ==> do
                  transmitted += 1
                  emitV txd true
              , true ==> commandsErr += 1
              ]

  return ( snd acceptChan
         , fst cmdModeChan
         , BackpressureTransmit (fst cmdChan) (snd txdoneChan)
         )
