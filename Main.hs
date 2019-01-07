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
import           System.Log.Logger
import           Data.Semigroup                 ( (<>) )

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


runProgram = do
    initializeLogger
    install


install :: ReaderT Config IO ()
install = do
    installDependencies
    Symlinks.createSymlinks
    installCustomScripts
    liftIO $ runProcess
        "defaults write com.apple.Dock autohide-delay -float 5 && killall Dock"
        []
    return ()
  where
    installDependencies = do
        Config { includeDependencies } <- ask
        when includeDependencies Dependencies.install

    installCustomScripts = do
        Config { includeCustomScripts } <- ask
        when includeCustomScripts CustomScripts.install
