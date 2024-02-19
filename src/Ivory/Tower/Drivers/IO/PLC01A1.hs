{-# LANGUAGE DataKinds #-}

module Ivory.Tower.Drivers.IO.PLC01A1 (
    plcTower
  , module Ivory.Tower.Drivers.IO.CLT01
  , module Ivory.Tower.Drivers.IO.VNI8200
  ) where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.Bus.Sched
import Ivory.Tower.HAL.Bus.SPI.DeviceHandle

import Ivory.Tower.Drivers.IO.CLT01
import Ivory.Tower.Drivers.IO.VNI8200

import Ivory.BSP.STM32.Peripheral.GPIO

-- | Driver for X-NUCLEO-PLC01A1 board
-- composed of CLT01-38SQ7 and VNI8200XP
plcTower
  :: BackpressureTransmit
       ('Struct "spi_transaction_request")
       ('Struct "spi_transaction_result")
  -> ChanOutput ('Stored ITime) -- ^ SPI ready
  -> GPIOPin -- ^ VNI8200 enable pin
  -> Tower
       e
       ( ChanOutput ('Struct "clt_status")
       , ChanInput ('Array 8 (Stored IBool))
       , ChanOutput ('Struct "vni_status")
       )
plcTower sreq sready enablePin = do
  (inTask, inReq) <- task "in"
  inputsStatus <-
    cltTower
      inReq
      sready
      (SPIDeviceHandle 0)

  (outTask, outReq) <- task "out"
  (setOutput, outStatus) <-
    vniTower
      outReq
      sready
      (SPIDeviceHandle 1)
      enablePin
      Nothing -- watchdog pin is triggered by CLT chipselect pin

  schedule
    "plc01a_scheduler"
    [ inTask
    , outTask
    ]
    sready
    sreq

  pure
    ( inputsStatus
    , setOutput
    , outStatus
    )
