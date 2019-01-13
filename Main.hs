module Main where

import qualified CustomScripts
import qualified Dependencies
import           Options.Applicative
import           Control.Monad.Reader
import           Config

opts :: ParserInfo Options
opts = info
    (liftA2 Options includeCustomScriptsFlag debugModeFlag <**> helper)
    (  fullDesc
    <> progDesc
           "Sets up all dependencies and symlinks necessary for vim/tmux workflow"
    <> header "Vim/Tmux dependency installation"
    )

includeCustomScriptsFlag :: Parser Bool
includeCustomScriptsFlag =
    switch $ long "include-custom-scripts" <> short 's' <> help
        "Compile and install custom scripts"

debugModeFlag :: Parser Bool
debugModeFlag = switch $ long "debug" <> short 'd' <> help "debug mode"

main :: IO ()
main = do
    config <- configFromOptions =<< execParser opts
    runReaderT runProgram config
    return ()


runProgram :: ReaderT Config IO ()
runProgram = do
    initializeLogger
    install


install :: ReaderT Config IO ()
install = do
    Dependencies.install
    installCustomScripts
  where
    installCustomScripts = do
        Config { includeCustomScripts } <- ask
        when includeCustomScripts CustomScripts.install
