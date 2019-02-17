module OpenSource
    ( clone
    )
where

import qualified System.Directory              as Dir
import           System.Process                 ( callCommand )
import           Control.Monad

repos = ["react-elm", "river"]

cloneProject openSourceDir repo = do
    repoExists <- Dir.doesPathExist $ openSourceDir <> "/" <> repo
    unless
        repoExists
        (  callCommand
        $  "git clone git@github.com:DerekMaffett/"
        <> repo
        <> ".git"
        )

clone forceRemove = do
    openSourceDir <- getOpenSourceDir
    when forceRemove $ Dir.removePathForcibly openSourceDir
    Dir.createDirectoryIfMissing True openSourceDir
    Dir.withCurrentDirectory openSourceDir
        $ mapM_ (cloneProject openSourceDir) repos
  where
    getOpenSourceDir = do
        homeDir <- Dir.getHomeDirectory
        return $ homeDir <> "/projects/open-source"
