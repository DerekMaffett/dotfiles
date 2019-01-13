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
    runProcess ("mkdir -p " <> dir)


install = do
    Config { installationsDir, buildDir, binDir, builtConfigsDir } <- ask
    mapM_ resetDirectory [installationsDir, buildDir, binDir, builtConfigsDir]
    Packages.install
    return ()
