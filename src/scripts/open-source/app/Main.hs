module Main where

import qualified OpenSource
import           Options.Applicative
import           Control.Monad

data Command
  = Clone Bool

cloneOpts :: ParserInfo Bool
cloneOpts = info
    (switch (long "force" <> short 'f' <> help "force re-clone") <**> helper)
    (fullDesc <> progDesc "Clones all relevant open source projects" <> header
        "open source project cloning"
    )

opts :: ParserInfo Command
opts = info optsParser desc
  where
    optsParser = Clone <$> subparser (command "clone" cloneOpts) <**> helper
    desc       = fullDesc <> progDesc "open source specific commands"

main = do
    command <- execParser opts
    case command of
        Clone forceRemove -> OpenSource.clone forceRemove
    putStrLn "Done!"
