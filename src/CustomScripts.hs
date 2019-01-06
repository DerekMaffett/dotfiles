module CustomScripts
    ( install
    )
where

import qualified System.Directory              as Dir
import           System.Process
import           Data.Semigroup                 ( (<>) )

installScriptGlobally scriptPath =
    Dir.withCurrentDirectory scriptPath $ callCommand "stack install"

install = do
    currentDir  <- Dir.getCurrentDirectory
    scriptPaths <- getScriptPaths currentDir
    mapM_ installScriptGlobally scriptPaths
  where
    getScriptPaths currentDir =
        map (\scriptName -> currentDir <> "/src/scripts/" <> scriptName)
            <$> Dir.listDirectory (currentDir <> "/src/scripts")
