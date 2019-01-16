module Dependencies
    ( install
    )
where

import           Config
import           Process
import           Control.Monad.Reader
import qualified Packages



install = do
    Packages.install
    return ()
