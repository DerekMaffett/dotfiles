module Projects
    ( clone
    )
where

import qualified System.Directory              as Dir
import           System.Process                 ( callCommand )
import           Control.Monad
import           Data.Aeson

cloneProject projectsDir repo = do
    repoExists <-
        Dir.doesPathExist $ projectsDir <> "/" <> (dropWhile (/= '/') repo)
    unless repoExists (callCommand $ "git clone " <> repo <> ".git")

clone forceRemove = do
    projectsDir <- getProjectsDir

    when forceRemove $ Dir.removePathForcibly projectsDir
    Dir.createDirectoryIfMissing True projectsDir

    repos <- getRepos
    Dir.withCurrentDirectory projectsDir
        $ mapM_ (cloneProject projectsDir) repos
  where
    getProjectsDir = do
        homeDir <- Dir.getHomeDirectory
        return $ homeDir <> "/projects"
    getRepos = do
        homeDir      <- Dir.getHomeDirectory
        publicRepos  <- decodeFileStrict $ homeDir <> "/.projects.json"
        privateRepos <- decodeFileStrict $ homeDir <> "/.work-projects.json"
        return $ publicRepos <> privateRepos
