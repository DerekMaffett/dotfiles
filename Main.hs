module Main where

import qualified CustomScripts
import qualified Dependencies
import qualified Symlinks
import           Options.Applicative
import           Control.Monad.Reader
import           Control.Monad
import           Config
import           Process
import           Logger

opts :: ParserInfo Options
opts = info
    (liftA2 Options includeDependenciesFlag includeCustomScriptsFlag <**> helper
    )
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
    config <- configFromOptions =<< execParser opts
    runReaderT runProgram config
    return ()


runProgram = do
    initializeLogger
    install


install :: ReaderT Config IO String
install = do
    installDependencies
    Symlinks.createSymlinks
    installCustomScripts
    runProcess
        "defaults write com.apple.Dock autohide-delay -float 5 && killall Dock"
        []
  where
    installDependencies = do
        Config { includeDependencies } <- ask
        when includeDependencies Dependencies.install

    installCustomScripts = do
        Config { includeCustomScripts } <- ask
        when includeCustomScripts CustomScripts.install
