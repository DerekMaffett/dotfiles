module Homebrew
    ( install
    )
where

import           Process
import           Logger
import           Safe                           ( headMay )
import           Data.List
import           Control.Monad
import           Data.Maybe

brewPrograms =
    [ "autojump"
    , "neovim"
    , "cloc"
    , "tmux"
    , "the_silver_searcher"
    , "python"
    , "rbenv"
    ]

brewUpgrade package = do
    logNotice $ "Upgrading " <> package <> "..."
    runProcess ("brew upgrade " <> package) []


brewInstall package = do
    logNotice $ "Installing " <> package <> "..."
    runProcess ("brew install " <> package) []

install = do
    logNotice "Updating Homebrew..."
    runProcess "brew update" []
    uninstalledPackages <- getUninstalled <$> runProcess "brew list" []
    outdatedPackages    <- getOutdated <$> runProcess "brew outdated" []
    logDebug $ "Uninstalled packages: " <> show uninstalledPackages
    logDebug $ "Outdated packages: " <> show outdatedPackages
    mapM_ brewInstall uninstalledPackages
    mapM_ brewUpgrade outdatedPackages
    when (null uninstalledPackages && null outdatedPackages)
        $ logNotice "Nothing to install/upgrade!"
  where
    getUninstalled = (\installed -> nub brewPrograms \\ nub installed) . words
    getOutdated    = catMaybes . (map headMay) . (map words) . lines
