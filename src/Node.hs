module Node
    ( install
    , installPackages
    )
where

import           Logger
import           Process

nvmGlobalVersion = "v10.15.0"
globalNpmPackages = ["elm", "elm-format"]


npmInstall package = do
    logNotice $ "Installing " <> package <> "..."
    runProcess ("source ~/.nvm/nvm.sh && npm install -g " <> package) []

installPackages = mapM_ npmInstall globalNpmPackages

install = do
    logNotice "Installing NVM..."
    runProcess
        "curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.11/install.sh | zsh"
        []
    runProcess ("source ~/.nvm/nvm.sh && nvm install " <> nvmGlobalVersion) []
