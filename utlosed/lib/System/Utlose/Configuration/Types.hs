{-
description: Global configuration types for utlosed
-}
module System.Utlose.Configuration.Types
( -- * Datatypes
  App(..)
, Permissions(..)
, Interconnects(..)
, Interconnect(..)
) where

import Net.IP
import Data.Word

-- | Global app settings
data App = App
  { instanceName   :: !String
  , defaultTimeout :: !Word
  , serviceFiles   :: [FilePath]
  , logFolder      :: !FilePath
  , publicIP       :: Maybe (IP, Word)
  , permissions    :: !Permissions
  , interconnects  :: Interconnects
  }

-- | How much control parent instences can have
data Permissions = Permissions
  { canViewStatus :: !Bool
  , canControl    :: !Bool
  , canViewLogs   :: !Bool
  , visibility    :: !Word
  }

-- | Settings for interconnected instances
data Interconnects = Interconnects
  { instancePrivateKey :: !String -- TODO: Look into cryptonite
  , resourceLimits     :: [String]
  , children           :: [Interconnect]
  }

data Interconnect = Interconnect
  { childPublicKey :: !String -- TODO: Look into cryptonite
  , childIP        :: !IP
  }
