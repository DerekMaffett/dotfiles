module Main where

import qualified Dependencies
import qualified Symlinks
import           Options.Applicative
import           Control.Monad
import           Data.Semigroup                 ( (<>) )
import           System.Process

opts :: ParserInfo Bool
opts =
  info (includeDependenciesFlag <**> helper)
    $  fullDesc
    <> progDesc
         "Sets up all dependencies and symlinks necessary for vim/tmux workflow"
    <> header "Vim/Tmux dependency installation"

includeDependenciesFlag :: Parser Bool
includeDependenciesFlag =
  switch $ long "dependencies" <> short 'd' <> help "Install full dependencies"

main :: IO ()
main = do
  fullInstall <- execParser opts
  unless fullInstall
    $ putStrLn
        "Skipping dependency installation... use --dependencies to install everything"
  when fullInstall $ Dependencies.installDependencies
  Symlinks.createSymlinks
  putStrLn "Symlinks created!"
  callCommand
    "defaults write com.apple.Dock autohide-delay -float 5 && killall Dock"
