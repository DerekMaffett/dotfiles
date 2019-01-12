module Registry.Zsh
    ( setShell
    )
where

import           Process
import           Logger
import           Config
import           Control.Monad
import           Control.Monad.Reader
import           Data.List

setShell = do
    output <- dropWhileEnd (== '\n') <$> runProcess "echo $SHELL"
    logDebug $ "Current shell: \"" <> output <> "\""
    when (output /= "/bin/zsh") (runProcess' "chsh -s $(which zsh)")
