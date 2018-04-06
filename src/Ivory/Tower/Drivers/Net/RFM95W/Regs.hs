{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ivory.Tower.Drivers.Net.RFM95W.Regs where

import Ivory.Tower.Drivers.Net.RFM95W.RegTypes
import Ivory.Language

[ivory|
  bitdata RFM95Transaction :: Bits 16 = rfm_dat
   { rfm_rw :: RFMRW
   , rfm_addr :: RFMAddr
   , rfm_data :: Bits 8 }

  bitdata OP_MODE :: Bits 8 = rfm_op_mode_reg
   { rfm_op_mode_long_range_mode       :: Bit
   , rfm_op_mode_access_shared_reg     :: Bit    -- Allows access to FSK registers when in LoRa mode
   , _                                 :: Bits 2
   , rfm_op_mode_low_frequency_mode_on :: Bit
   , rfm_op_mode_mode                  :: Mode
   }

  bitdata FR_MSB :: Bits 8 = rfm_fr_msb_reg
  { rfm_fr_msb_frf :: Bits 8 -- Frf(23:16)
  }

  bitdata FR_MID :: Bits 8 = rfm_fr_mid_reg
  { rfm_fr_mid_frf :: Bits 8 -- Frf(15:8)
  }

  bitdata FR_LSB :: Bits 8 = rfm_fr_lsb_reg
  { rfm_fr_lsb_frf :: Bits 8 -- Frf(7:0)
  }

  bitdata PA_CONFIG :: Bits 8 = rfm_pa_config_reg
  { rfm_pa_config_pa_select    :: Bit
  , rfm_pa_config_max_power    :: Bits 3
  , rfm_pa_config_output_power :: Bits 4
  }

  bitdata PA_RAMP :: Bits 8 = rfm_pa_ramp_reg
  { _            :: Bits 4
  , rfm_pa_ramp  :: PaRamp -- Rise/Fall time of ramp up/down in FSK
  }

  bitdata OCP :: Bits 8 = rfm_ocp_reg
  { _                :: Bits 2
  , rfm_ocp_ocp_on   :: Bit     -- Enables overload current protection (OCP) for PA
  , rfm_ocp_ocp_trim :: Bits 5  -- Trimming of OCP current
  }

  bitdata LNA_GAIN :: Bits 8 = rfm_lna_gain_reg
  { rfm_lna_gain_lna_gain     :: LnaGain
  , rfm_lna_gain_lna_boost_lf :: Bits 2
  , _                         :: Bit
  , rfm_lna_gain_lna_boost_hf :: Bits 2
  }

  -- SPI interface address pointer in FIFO data buffer.
  bitdata FIFO_ADDR_PTR :: Bits 8 = rfm_fifo_addr_ptr_reg
  { rfm_fifo_addr_ptr :: Bits 8 }

  -- write base address in FIFO data buffer for TX modulator
  bitdata FIFO_TX_BASE_ADDR :: Bits 8 = rfm_fifo_tx_base_addr_reg
  { rfm_fifo_tx_base_addr :: Bits 8 }

  -- read base address in FIFO data buffer for RX demodulator
  bitdata FIFO_RX_BASE_ADDR :: Bits 8 = rfm_fifo_rx_base_addr_reg
  { rfm_fifo_rx_base_addr :: Bits 8 }

  -- Start address (in data buffer) of last packet received
  bitdata FIFO_RX_CURRENT_ADDR :: Bits 8 = rfm_fifo_tx_current_addr_reg
  { rfm_fifo_rx_current_addr :: Bits 8 }

  bitdata IRQ_FLAGS_MASK :: Bits 8 = rfm_irq_flags_mask_reg
  { rfm_irq_flags_mask_rx_timeout_mask          :: Bit
  , rfm_irq_flags_mask_rx_done_mask             :: Bit
  , rfm_irq_flags_mask_payload_crc_error_mask   :: Bit
  , rfm_irq_flags_mask_valid_header_mask        :: Bit
  , rfm_irq_flags_mask_tx_done_mask             :: Bit
  , rfm_irq_flags_mask_cad_done_mask            :: Bit
  , rfm_irq_flags_mask_fhss_change_channel_mask :: Bit
  , rfm_irq_flags_mask_cad_detected_mask        :: Bit
  }

  bitdata IRQ_FLAGS :: Bits 8 = rfm_irq_flags_reg
  { rfm_irq_flags_rx_timeout          :: Bit
  , rfm_irq_flags_rx_done             :: Bit
  , rfm_irq_flags_payload_crc_error   :: Bit
  , rfm_irq_flags_valid_header        :: Bit
  , rfm_irq_flags_tx_done             :: Bit
  , rfm_irq_flags_cad_done            :: Bit
  , rfm_irq_flags_fhss_change_channel :: Bit
  , rfm_irq_flags_cad_detected        :: Bit
  }

  -- Number of payload bytes of latest packet received
  bitdata RX_NB_BYTES :: Bits 8 = rfm_rx_nb_bytes_reg
  { rfm_rx_nb_bytes_fifo :: Bits 8 }

  bitdata VALID_HEADER_CNT_MSB :: Bits 8 = rfm_valid_header_cnt_msb_reg
  { rfm_valid_header_cnt_msb :: Bits 8 }

  bitdata VALID_HEADER_CNT_LSB :: Bits 8 = rfm_valid_header_cnt_lsb_reg
  { rfm_valid_header_cnt_lsb :: Bits 8 }

  bitdata VALID_PACKET_CNT_MSB :: Bits 8 = rfm_valid_packet_cnt_msb_reg
  { rfm_valid_packet_cnt_msb :: Bits 8 }

  bitdata VALID_PACKET_CNT_LSB :: Bits 8 = rfm_valid_packet_cnt_lsb_reg
  { rfm_valid_packet_cnt_lsb :: Bits 8 }

  bitdata MODEM_STAT :: Bits 8 = rfm_modem_stat_reg
  { rfm_modem_stat_rx_coding_rate      :: Bits 3 -- Coding rate of last header received, XXX: make it a bitdata
  , rfm_modem_stat_modem_clear         :: Bit
  , rfm_modem_stat_header_info_valid   :: Bit
  , rfm_modem_stat_rx_ongoing          :: Bit
  , rfm_modem_stat_signal_synchronized :: Bit
  , rfm_modem_stat_signal_detected     :: Bit
  }

  -- Estimation of SNR on last packet received.In two’s compliment
  -- format mutiplied by 4
  bitdata PKT_SNR_VALUE :: Bits 8 = rfm_pkt_snr_value_reg
  { rfm_snr_value_packet_snr :: Bits 8 }

  -- RSSI of the latest packet received (dBm)
  bitdata PKT_RSSI_VALUE :: Bits 8 = rfm_pkt_rssi_value_reg
  { rfm_pkt_rssi_value_packet_rssi :: Bits 8 }

  -- Current RSSI value (dBm)
  bitdata RSSI_VALUE :: Bits 8 = rfm_rssi_value_reg
  { rfm_rssi_value_rssi :: Bits 8 }

  bitdata HOP_CHANNEL :: Bits 8 = rfm_hop_channel_reg
  { rfm_hop_channel_pll_timeout          :: Bit
  , rfm_hop_channel_rx_payload_crc_on    :: Bit
  , rfm_hop_channel_fhss_present_channel :: Bits 6
  }

  bitdata MODEM_CONFIG1 :: Bits 8 = rfm_modem_config1_reg
  { rfm_modem_config1_bw                      :: Bandwidth
  , rfm_modem_config1_coding_rate             :: Bits 3
  , rfm_modem_config1_implicit_header_mode_on :: Bit
  }

  bitdata MODEM_CONFIG2 :: Bits 8 = rfm_modem_config2_reg
  { rfm_modem_config2_spreading_factor   :: SpreadingFactor
  , rfm_modem_config2_tx_continuous_mode :: Bit
  , rfm_modem_config2_rx_payload_crc_on  :: Bit
  , rfm_modem_config2_symb_timeout_msb   :: Bits 2 -- SymbTimeout(9:8)
  }

  -- RX operation time-out value expressed as number of symbols:
  -- Timeout = SymbTimeout * Ts
  -- XXX: wtf is Ts?
  bitdata SYMB_TIMEOUT_LSB :: Bits 8 = rfm_symb_timeout_lsb_reg
  { rfm_symb_timeout_lsb :: Bits 8 }

  bitdata PREAMBLE_LENGTH_MSB :: Bits 8 = rfm_preamble_msb_reg
  { rfm_premble_length_msb :: Bits 8 }

  bitdata PREAMBLE_LENGTH_LSB :: Bits 8 = rfm_preamble_lsb_reg
  { rfm_premble_length_lsb :: Bits 8 }

  -- Payload length in bytes. The register needs to be set in implicit
  -- header mode for the expected packet length. A 0 value is not permitted
  bitdata PAYLOAD_LENGTH :: Bits 8 = rfm_payload_length_reg
  { rfm_payload_length :: Bits 8 }

  bitdata MAX_PAYLOAD_LENGTH :: Bits 8 = rfm_max_payload_length_reg
  { rfm_max_payload_length :: Bits 8 }

  bitdata HOP_PERIOD :: Bits 8 = rfm_hop_period_reg
  { rfm_hop_period_freq :: Bits 8 }

  -- Current value of RX databuffer pointer (address of last byte
  -- written by Lora receiver)
  bitdata FIFO_RX_BYTE_ADDR :: Bits 8 = rfm_fifo_rx_byte_addr_reg
  { rfm_fifo_rx_byte_fifo_rx_byte_addr_ptr :: Bits 8 }

  bitdata MODEM_CONFIG3 :: Bits 8 = rfm_modem_config3_reg
  { _ :: Bits 4
  , rfm_modem_config3_mobile_node :: Bit
  , rfm_modem_config3_agc_auto_on :: Bit
  , _ :: Bits 2
  }

|]

