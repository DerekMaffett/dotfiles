module Main where

import qualified CustomScripts
import qualified Dependencies
import qualified Symlinks
import           Options.Applicative
import           Control.Monad
import           Data.Semigroup                 ( (<>) )
import           System.Process

data Options = Options
  { includeDependencies :: Bool
  , includeCustomScripts :: Bool
  }

opts :: ParserInfo Options
opts = info
  (liftA2 Options includeDependenciesFlag includeCustomScriptsFlag <**> helper)
  (  fullDesc
  <> progDesc
       "Sets up all dependencies and symlinks necessary for vim/tmux workflow"
  <> header "Vim/Tmux dependency installation"
  )

includeCustomScriptsFlag :: Parser Bool
includeCustomScriptsFlag =
  switch $ long "include-custom-scripts" <> short 's' <> help
    "Compile and install custom scripts"

includeDependenciesFlag :: Parser Bool
includeDependenciesFlag =
  switch $ long "include-dependencies" <> short 'd' <> help
    "Install full dependencies"

main :: IO ()
main = do
  Options { includeDependencies, includeCustomScripts } <- execParser opts
  unless
    includeDependencies
    (putStrLn
      "Skipping dependency installation... use --include-dependencies to install everything"
    )
  when includeDependencies Dependencies.installDependencies
  Symlinks.createSymlinks
  unless
    includeCustomScripts
    (putStrLn
      "Skipping custom script compilation... use --include-custom-scripts to compile"
    )
  when includeCustomScripts CustomScripts.install
  putStrLn "Symlinks created!"
  callCommand
    "defaults write com.apple.Dock autohide-delay -float 5 && killall Dock"
