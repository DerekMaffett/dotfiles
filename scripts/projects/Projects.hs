module Projects
    ( clone
    , new
    )
where

import qualified System.Directory              as Dir
import           System.Exit
import           System.Process                 ( callCommand )
import           Files                          ( alterJsonFile
                                                , alterYamlFile
                                                , readJsonFile
                                                )
import           Tmuxinator                     ( addToTmuxinatorWorkspace
                                                , WindowConfig(..)
                                                )
import           Control.Monad
import           Data.Aeson
import           Data.HashMap.Strict


new projectName = do
    projectsDir <- getProjectsDir
    createProjectDirectory projectsDir projectName
    alterYamlFile "dotfiles/configs/tmuxinator/open-source.yml"
        $ (addToTmuxinatorWorkspace projectName $ WindowConfig
              { root   = "~/projects/" <> projectName
              , layout = "main-vertical"
              , panes  = [Just "vim", Nothing, Nothing]
              }
          )

clone forceRemove = do
    projectsDir <- getProjectsDir

    when forceRemove $ Dir.removePathForcibly projectsDir
    Dir.createDirectoryIfMissing True projectsDir

    repos <- getRepos
    Dir.withCurrentDirectory projectsDir
        $ mapM_ (cloneProject projectsDir) repos
  where
    getRepos = do
        homeDir                  <- Dir.getHomeDirectory
        publicRepos :: [String]  <- readJsonFile publicProjectsFilePath
        privateRepos :: [String] <- readJsonFile privateProjectsFilePath
        return $ publicRepos <> privateRepos

createProjectDirectory projectsDir projectName =
    Dir.createDirectoryIfMissing True $ projectsDir <> "/" <> projectName

cloneProject projectsDir repo = do
    repoExists <-
        Dir.doesPathExist $ projectsDir <> "/" <> (dropWhile (/= '/') repo)
    unless repoExists (callCommand $ "git clone " <> repo <> ".git")

publicProjectsFilePath = ".config/projects/.projects.json"
privateProjectsFilePath = ".config/projects/.work-projects.json"

getProjectsDir = do
    homeDir <- Dir.getHomeDirectory
    return $ homeDir <> "/projects"
