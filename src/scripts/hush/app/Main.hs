module Main where

import qualified Projects
import           Options.Applicative
import           Control.Monad

data Command
  = Clone Bool

cloneOpts :: ParserInfo Bool
cloneOpts = info
    (switch (long "force" <> short 'f' <> help "force re-clone") <**> helper)
    (fullDesc <> progDesc "Clones all relevant projects" <> header
        "project cloning"
    )

opts :: ParserInfo Command
opts = info optsParser desc
  where
    optsParser = Clone <$> subparser (command "clone" cloneOpts) <**> helper
    desc       = fullDesc <> progDesc "project management commands"

main = do
    command <- execParser opts
    case command of
        Clone forceRemove -> Projects.clone forceRemove
    putStrLn "Done!"
