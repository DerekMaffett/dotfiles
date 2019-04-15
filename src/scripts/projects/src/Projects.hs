module Projects
    ( clone
    )
where

import qualified System.Directory              as Dir
import           System.Process                 ( callCommand )
import           System.Exit
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
        homeDir           <- Dir.getHomeDirectory
        publicReposResult <-
            eitherDecodeFileStrict $ homeDir <> "/.projects.json"
        privateReposResult <-
            eitherDecodeFileStrict $ homeDir <> "/.work-projects.json"
        case extractParsedResults publicReposResult privateReposResult of
            Left  msg   -> die msg
            Right repos -> return repos
    extractParsedResults publicReposResult privateReposResult = do
        publicRepos  <- publicReposResult
        privateRepos <- privateReposResult
        return $ (publicRepos :: [String]) <> (privateRepos :: [String])
