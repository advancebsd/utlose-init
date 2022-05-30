module Main where

import System.Utlose
import System.Utlose.Configuration

import Control.Monad.Reader
import Control.Monad.IO.Class

import qualified Data.Text.IO as T

type AppM a = ReaderT App IO a

configuredMain :: AppM ()
configuredMain = liftIO $ putStrLn "Hello World"

main :: IO ()
main = do
  app <- readConfig "/etc/utlose/utlosed.toml"
  case app of
    Right conf -> runReaderT configuredMain conf
    Left  err  -> T.putStrLn err


