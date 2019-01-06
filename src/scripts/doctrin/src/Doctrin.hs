module Doctrin
    ( clone
    )
where

import qualified System.Directory              as Dir
import           System.Process                 ( callCommand )
import           Control.Monad

repos =
    [ "anamnesis-report-renderer"
    , "flow-app"
    , "start-client"
    , "start-server"
    , "start-questionnaire-components"
    , "react-elm"
    , "battrebesok"
    , "river"
    ]

cloneProject doctrinDir repo = do
    repoExists <- Dir.doesPathExist $ doctrinDir <> "/" <> repo
    unless
        repoExists
        (callCommand $ "git clone git@bitbucket.org:doctrin/" <> repo <> ".git")

clone forceRemove = do
    doctrinDir <- getDoctrinDir
    when forceRemove $ Dir.removePathForcibly doctrinDir
    Dir.createDirectoryIfMissing True doctrinDir
    Dir.withCurrentDirectory doctrinDir $ mapM_ (cloneProject doctrinDir) repos
  where
    getDoctrinDir = do
        homeDir <- Dir.getHomeDirectory
        return $ homeDir <> "/projects/doctrin"
