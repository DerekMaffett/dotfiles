module Registry.Node
    ( install
    )
where

import           Logger
import           Process

nvmGlobalVersion = "v10.15.0"


install = do
    logNotice "Installing NVM..."
    runProcess
        "curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.34.0/install.sh | zsh"
    runProcess (". ~/.nvm/nvm.sh && nvm install " <> nvmGlobalVersion)
    runProcess' $ ". ~/.nvm/nvm.sh && nvm use " <> nvmGlobalVersion
