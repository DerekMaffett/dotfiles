module CustomScripts
    ( install
    )
where

import qualified System.Directory              as Dir
import           Control.Monad.Reader
import           Process
import           Config

installScriptGlobally scriptPath =
    Dir.withCurrentDirectory scriptPath $ runProcess "stack install" []

install :: ReaderT Config IO ()
install = do
    scriptsDir <-
        (\Config { dotfilesDir } -> dotfilesDir <> "/src/scripts") <$> ask
    scriptPaths <- liftIO $ getScriptPaths scriptsDir
    liftIO $ mapM_ installScriptGlobally scriptPaths
  where
    getScriptPaths scriptsDir =
        map (\scriptName -> scriptsDir <> "/" <> scriptName)
            <$> Dir.listDirectory scriptsDir
