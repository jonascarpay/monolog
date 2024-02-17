{-# LANGUAGE StrictData #-}

module Monolog.Types
  ( Note (..),
  )
where

import Data.Aeson
import Data.Text (Text)
import Data.Time
import Data.UUID (UUID)
import GHC.Generics (Generic)

data Note = Note
  { id :: UUID,
    createTime :: ZonedTime,
    archiveTime :: Maybe ZonedTime,
    body :: Text -- TODO newtype to guarantuee stripping
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)
