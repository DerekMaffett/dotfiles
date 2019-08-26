module Main where

import qualified Projects
import           Options.Applicative
import           Control.Monad

data Command
  = Clone Bool
  | New String

cloneOpts :: ParserInfo Command
cloneOpts = Clone <$> info
    (switch (long "force" <> short 'f' <> help "force re-clone") <**> helper)
    (fullDesc <> progDesc "Clones all relevant projects" <> header
        "project cloning"
    )

newOpts :: ParserInfo Command
newOpts = New <$> info
    (strOption $ long "name" <> short 'n' <> metavar "PROJECT_NAME" <> help
        "project name"
    )
    (fullDesc <> progDesc "Creates a new project" <> header "Project creation")

opts :: ParserInfo Command
opts = info optsParser desc
  where
    optsParser =
        subparser (command "clone" cloneOpts <> command "new" newOpts)
            <**> helper
    desc = fullDesc <> progDesc "project management commands"

main = do
    command <- execParser opts
    case command of
        Clone forceRemove -> Projects.clone forceRemove
        New   name        -> Projects.new name

    putStrLn "Done!"
