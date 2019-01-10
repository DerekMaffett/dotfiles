module Dependencies
    ( install
    )
where

import           Config
import           Process
import           Control.Monad.Reader
import qualified Packages


resetDirectory dir = do
    runProcess ("rm -rf " <> dir)
    runProcess ("mkdir " <> dir)


install = do
    runProcess "cd ./src && pwd"
    Config { installationsDir, buildDir, binDir } <- ask
    mapM_ resetDirectory [installationsDir, buildDir, binDir]
    Packages.install
    return ()
