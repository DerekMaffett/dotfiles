module Symlinks
    ( createSymlinks
    , createSymlink
    )
where

import           System.Directory              as Dir
import           Control.Monad.Reader
import           Config
import           Logger
import           Data.List
import           Control.Monad

homeDirConfigs =
    [ "gitconfig"
    , "zshrc"
    , "zprofile"
    , "shell"
    , "vimrc"
    , "tmux.conf"
    , "agignore"
    , "prettierrc.js"
    , "prettierignore"
    ]


friendlyPath homeDir path = case stripPrefix homeDir path of
    Just homeBasedPath -> "~" <> homeBasedPath
    Nothing            -> path


logSymlink targetPath linkPath = do
    Config { homeDir } <- ask
    logNotice
        $  (friendlyPath homeDir targetPath)
        <> " -> "
        <> (friendlyPath homeDir linkPath)


parentOf = dropWhileEnd (/= '/')


createSymlink_ targetPath linkPath = do
    exists <- Dir.doesPathExist linkPath
    when exists $ Dir.removeFile linkPath
    Dir.createDirectoryIfMissing True (parentOf linkPath)
    Dir.createFileLink targetPath linkPath


createSymlink targetPath linkPath = do
    liftIO $ createSymlink_ targetPath linkPath
    logSymlink targetPath linkPath


linkToHomeDir :: String -> ReaderT Config IO ()
linkToHomeDir configName = do
    Config { homeDir, configsDir } <- ask
    createSymlink (configsDir <> "/" <> configName)
                  (homeDir <> "/." <> configName)


createSymlinks :: ReaderT Config IO ()
createSymlinks = do
    Config { homeDir, configsDir } <- ask
    logNotice "Creating Symlinks..."
    mapM_ linkToHomeDir homeDirConfigs
    createSymlink (configsDir <> "/init.vim")
                  (homeDir <> "/.config/nvim/init.vim")
    createSymlink (configsDir <> "/brittany.yaml")
                  (homeDir <> "/.config/brittany/config.yaml")
