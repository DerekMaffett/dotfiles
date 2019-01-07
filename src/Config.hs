module Config
    ( Config(..)
    , Options(..)
    , configFromOptions
    , initializeLogger
    )
where

import qualified System.Directory              as Dir
import           System.Log.Logger
import           Control.Monad.Reader

-- data Path = Path String
--
-- instance Functor Path where
--   fmap f (Path path) = Path (f path)

data Config = Config
  { logger :: String
  , includeDependencies :: Bool
  , includeCustomScripts :: Bool
  , homeDir :: String
  , dotfilesDir :: String
  , configsDir :: String
  }

data Options = Options
  { includeDependencies :: Bool
  , includeCustomScripts :: Bool
  }

configFromOptions :: Options -> IO Config
configFromOptions Options { includeDependencies, includeCustomScripts } = do
    homeDir <- Dir.getHomeDirectory
    return Config
        { logger               = "BasicLogger"
        , includeDependencies  = includeDependencies
        , includeCustomScripts = includeCustomScripts
        , homeDir              = homeDir
        , dotfilesDir          = getDotfilesDir homeDir
        , configsDir           = getConfigsDir homeDir
        }
  where
    getDotfilesDir homeDir = homeDir <> "/dotfiles"
    getConfigsDir homeDir = (getDotfilesDir homeDir) <> "/src/configs"

initializeLogger :: ReaderT Config IO ()
initializeLogger = do
    Config { logger } <- ask
    liftIO $ updateGlobalLogger logger $ setLevel (level logger)
  where
    level logger = case logger of
        "BasicLogger" -> NOTICE
        "DebugLogger" -> DEBUG
        _             -> NOTICE
