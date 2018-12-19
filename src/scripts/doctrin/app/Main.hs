module Main where

import qualified Doctrin
import           Options.Applicative
import           Control.Monad

data Command
  = Clone

cloneOpts :: ParserInfo a
cloneOpts = info
  empty
  (fullDesc <> progDesc "Clones all relevant Doctrin projects" <> header
    "Doctrin project cloning"
  )

opts :: ParserInfo Command
opts = info optsParser desc
 where
  optsParser =
    const Clone <$> subparser (command "clone" cloneOpts) <**> helper
  desc = fullDesc <> progDesc "Doctrin-specific commands"

main = do
  command <- execParser opts
  case command of
    Clone -> Doctrin.clone
