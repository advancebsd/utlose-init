{-|
Description: Data types for command line arguments
-}

module CLI.Types
( -- * Top Level Options
  Options(..)
, Command(..)
  -- * Command Options
, MgrOptions(..)
, StatusOptions(..)
, LogsOptions(..)
, LintOptions(..)
, SnapOptions(..)
, RevertOptions(..)
, BootOptions(..)
, PatchOptions(..)
, QueryOptions(..)
  -- * Others
, Destination(..)
) where

-- | Common arguments for all commands
data Options = Args
  { optCommand :: Command
  , optAssert  :: String
  , optCheck   :: String
  }

-- | The individual commands
data Command = Start   MgrOptions
             | Stop    MgrOptions
             | Restart MgrOptions
             | Status  StatusOptions
             | Logs    LogsOptions
             | Lint    LintOptions
             | Snap    SnapOptions
             | Revert  RevertOptions
             | Enable  BootOptions
             | Disable BootOptions
             | Patch   PatchOptions
             | Query   QueryOptions


data MgrOptions = MgrOptions
  { mgrServiceNames :: [String] -- ^ Services we want to control
  }

data StatusOptions = StatusOptions
  { statServiceNames :: [String]
  }

data LogsOptions = LogsOptions
  { logsServices :: [String]
  }

data LintOptions = LintOptions
  { lintServices :: [String]
  }

data SnapOptions = SnapOptions
  { snapName :: Maybe String
  }

data RevertOptions = RevertOptions
  { revDestination :: Destination -- ^ Where are we jumping too
  }

data BootOptions = BootOptions
  { bootStart :: Bool
  }

data PatchOptions = PatchOptions
  {
  }

data QueryOptions = QueryOptions
  {
  }

-- | Where to revert to
data Destination = Previous     -- ^ Previous snapshot (default)
                 | Jump Int     -- ^ Jump back n snapshots
                 | Named String -- ^ Jumpt to named snapshots
