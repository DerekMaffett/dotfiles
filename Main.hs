module Main where

import qualified CustomScripts
import qualified Dependencies
import qualified Symlinks
import           Options.Applicative
import           Control.Monad.Reader
import           Control.Monad
import           Config
import           Process
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
    options <- execParser opts
    runReaderT install (configFromOptions options)


install :: ReaderT Config IO ()
install = do
    Config { includeDependencies, includeCustomScripts, logger } <- ask
    liftIO $ installDependencies includeDependencies
    liftIO Symlinks.createSymlinks
    liftIO $ noticeM logger "Symlinks created!"
    liftIO $ installCustomScripts includeCustomScripts
    liftIO $ runProcess
        "defaults write com.apple.Dock autohide-delay -float 5 && killall Dock"
        []
    return ()
  where
    installDependencies includeDependencies = if includeDependencies
        then Dependencies.install
        else
            putStrLn
                "Skipping dependency installation... use --include-dependencies to install everything"

    installCustomScripts includeCustomScripts = if includeCustomScripts
        then CustomScripts.install
        else
            putStrLn
                "Skipping custom script compilation... use --include-custom-scripts to compile"
