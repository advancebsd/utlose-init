{-# Language OverloadedStrings, LambdaCase #-}

module System.Utlose.Configuration
( module System.Utlose.Configuration.Types
, readConfig
, writeConfig
) where

import System.Utlose.Configuration.Types

import Toml (TomlCodec, (.=))
import qualified Toml
import qualified Net.IP as IP
import qualified Toml.Codec.Combinator.Custom as Toml
import qualified Data.Text as T
import Control.Monad
import Control.Monad.IO.Class

ipCodec = Toml.textBy IP.encode decodeIP
  where decodeIP t = case IP.decode t of
          Just ip -> Right ip
          Nothing -> Left $ t <> "Isn't a well formed IP address"


ipPortCodec kIP kPort = Toml.pair (ipCodec kIP) (Toml.word kPort)

optIpPort kIP kPort = Toml.dioptional $ ipPortCodec kIP kPort

appCodec :: TomlCodec App
appCodec = App
  <$>              Toml.string  "instance"        .= instanceName
  <*>              Toml.word    "default_timeout" .= defaultTimeout
  <*> Toml.arrayOf Toml._String "service_files"   .= serviceFiles
  <*>              Toml.string  "log_folder"      .= logFolder
  <*> optIpPort    "public_ip"  "public_port"     .= publicIP
  <*> Toml.table   permCodec    "permissions"     .= permissions
  <*> Toml.table   interCodec   "interconnects"   .= interconnects


permCodec = Permissions
  <$> Toml.bool "status"     .= canViewStatus
  <*> Toml.bool "control"    .= canControl
  <*> Toml.bool "logs"       .= canViewLogs
  <*> Toml.word "visibility" .= visibility


interCodec = Interconnects
  <$>              Toml.string  "key"             .= instancePrivateKey
  <*> Toml.arrayOf Toml._String "resource_limits" .= resourceLimits
  <*> Toml.list    interconnect "child"           .= children

interconnect = Interconnect
  <$> Toml.string "key" .= childPublicKey
  <*> ipCodec     "ip"  .= childIP


writeConfig :: (MonadIO m) => FilePath -> App -> m ()
writeConfig f = void . Toml.encodeToFile appCodec f

readConfig :: (MonadIO m) => FilePath -> m (Either T.Text App)
readConfig f = Toml.decodeFileEither appCodec f >>= \case
  Right x  -> return $ Right x
  Left err -> return $ Left $ Toml.prettyTomlDecodeErrors err
