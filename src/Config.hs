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

data Config = Config
  { logger :: String
  , includeCustomScripts :: Bool
  , rebuildConfigsOnly :: Bool
  , homeDir :: String
  , dotfilesDir :: String
  , devfilesDir :: String
  , installationsDir :: String
  , configsDir :: String
  , builtConfigsDir :: String
  , buildDir :: String
  , binDir :: String
  }

data Options = Options
  { includeCustomScripts :: Bool
  , rebuildConfigsOnly :: Bool
  , useDebugLogger :: Bool
  }

configFromOptions :: Options -> IO Config
configFromOptions Options { includeCustomScripts, rebuildConfigsOnly, useDebugLogger }
    = do
        homeDir <- Dir.getHomeDirectory
        return Config
            { logger = if useDebugLogger then "DebugLogger" else "BasicLogger"
            , includeCustomScripts = includeCustomScripts
            , rebuildConfigsOnly   = rebuildConfigsOnly
            , homeDir              = homeDir
            , installationsDir     = getInstallationsDir homeDir
            , dotfilesDir          = getDotfilesDir homeDir
            , devfilesDir          = getDevfilesDir homeDir
            , configsDir           = getConfigsDir homeDir
            , builtConfigsDir      = getBuiltConfigsDir homeDir
            , buildDir             = getBuildDir homeDir
            , binDir               = getBinDir homeDir
            }
  where
    getDotfilesDir homeDir = homeDir <> "/dotfiles"
    getDevfilesDir homeDir = (getDotfilesDir homeDir) <> "/.devfiles"
    getConfigsDir homeDir = (getDotfilesDir homeDir) <> "/src/configs"
    getBuiltConfigsDir homeDir = (getDevfilesDir homeDir) <> "/.configs"
    getInstallationsDir homeDir = (getDevfilesDir homeDir) <> "/.installations"
    getBuildDir homeDir = (getDevfilesDir homeDir) <> "/.build"
    getBinDir homeDir = (getDevfilesDir homeDir) <> "/.bin"

initializeLogger :: ReaderT Config IO ()
initializeLogger = do
    Config { logger } <- ask
    liftIO $ updateGlobalLogger logger $ setLevel (level logger)
  where
    level logger = case logger of
        "BasicLogger" -> NOTICE
        "DebugLogger" -> DEBUG
        _             -> NOTICE
