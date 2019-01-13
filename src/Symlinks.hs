module Symlinks
    ( createSymlink
    )
where

import           System.Directory              as Dir
import           Control.Monad.Reader
import           Config
import           Logger
import           Data.List
import           Control.Monad


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
