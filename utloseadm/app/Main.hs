module Main where

import Options.Applicative

import CLI
import CLI.Types

main :: IO ()
main = execParser opts >>= entry
  where opts = info (options <**> helper)
          ( fullDesc
         <> progDesc "Control an Utløse system"
          )

entry _ = putStrLn "Hello, Haskell!"
