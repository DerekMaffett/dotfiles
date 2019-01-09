module Dependencies
    ( install
    )
where

import qualified System.Directory              as Dir
import           System.Environment             ( lookupEnv )
import           Control.Monad
import           Config
import           Process
import qualified Zsh
import           Logger
import qualified TerminalHappiness
import           Control.Monad.Reader
import qualified Packages


logSection action = logNotice "" >> action >> logNotice ""

initInstallationsDir = do
    Config { installationsDir } <- ask
    runProcess ("rm -rf " <> installationsDir)
    runProcess ("mkdir " <> installationsDir)


install = do
    initInstallationsDir
    Packages.install
    TerminalHappiness.install
    return ()
