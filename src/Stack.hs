module Stack
    ( installPackages
    )
where

import           Process
import           Logger

stackPrograms = ["brittany"]

installPackages = mapM_ stackInstall stackPrograms

stackInstall package = do
    logNotice $ "Installing " <> package <> "..."
    runProcess ("stack install " <> package) []
