module Config
    ( Config(..)
    , Options(..)
    , configFromOptions
    , initializeLogger
    )
where

import qualified System.Directory              as Dir
import           System.Log.Logger
import           System.Exit                    ( ExitCode )
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
  , installationsDir :: String
  , dotfilesDir :: String
  , configsDir :: String
  , buildDir :: String
  , binDir :: String
  }

data Options = Options
  { includeDependencies :: Bool
  , includeCustomScripts :: Bool
  , useDebugLogger :: Bool
  }

configFromOptions :: Options -> IO Config
configFromOptions Options { includeDependencies, includeCustomScripts, useDebugLogger }
    = do
        homeDir <- Dir.getHomeDirectory
        return Config
            { logger = if useDebugLogger then "DebugLogger" else "BasicLogger"
            , includeDependencies  = includeDependencies
            , includeCustomScripts = includeCustomScripts
            , homeDir              = homeDir
            , installationsDir     = getInstallationsDir homeDir
            , dotfilesDir          = getDotfilesDir homeDir
            , configsDir           = getConfigsDir homeDir
            , buildDir             = getBuildDir homeDir
            , binDir               = getBinDir homeDir
            }
  where
    getDotfilesDir homeDir = homeDir <> "/dotfiles"
    getConfigsDir homeDir = (getDotfilesDir homeDir) <> "/src/configs"
    getInstallationsDir homeDir = (getDotfilesDir homeDir) <> "/.installations"
    getBuildDir homeDir = (getDotfilesDir homeDir) <> "/.build"
    getBinDir homeDir = (getDotfilesDir homeDir) <> "/.bin"

initializeLogger :: ReaderT Config IO ()
initializeLogger = do
    Config { logger } <- ask
    liftIO $ updateGlobalLogger logger $ setLevel (level logger)
  where
    level logger = case logger of
        "BasicLogger" -> NOTICE
        "DebugLogger" -> DEBUG
        _             -> NOTICE
