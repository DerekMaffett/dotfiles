module Doctrin
  ( clone
  )
where

import qualified System.Directory              as Dir
import           System.Process                 ( callCommand )

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

cloneProject repo =
  callCommand $ "git clone git@bitbucket.org:doctrin/" <> repo <> ".git"

clone = do
  homeDir <- Dir.getHomeDirectory
  Dir.createDirectoryIfMissing True $ homeDir <> "/projects/doctrin"
  Dir.withCurrentDirectory (homeDir <> "/projects/doctrin")
    $ mapM_ cloneProject repos
