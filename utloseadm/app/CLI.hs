module CLI
( options
) where

import Options.Applicative

import CLI.Types
import CLI.Commands

options :: Parser Options
options = Args
    <$> commands
    <*> strOption
        ( long    "assert"
       <> metavar "EXPR"
       <> help    "Precondition for commnad"
        )
    <*> strOption
        ( long    "check"
       <> metavar "EXPR"
       <> help    "Post condition (reverts if not)"
        )

commands :: Parser Command
commands = subparser
  ( startCommand
 <> stopCommand
 <> restartCommand
 <> statusCommand
 <> logsCommand
 <> lintCommand
 <> snapCommand
 <> revertCommand
 <> enableCommand
 <> disableCommand
 <> patchCommand
 <> queryCommand
  )

