module Node
    ( install
    )
where

import           Logger
import           Process

nvmGlobalVersion = "v10.15.0"


install = do
    logNotice "Installing NVM..."
    runProcess
        "curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.11/install.sh | zsh"
    runProcess ("source ~/.nvm/nvm.sh && nvm install " <> nvmGlobalVersion)
    return ()
