module Logger where

import           Config
import           System.Exit
import           System.Log.Logger
import           Control.Monad.Reader


logDebug :: String -> ReaderT Config IO ()
logDebug msg = do
    Config { logger } <- ask
    liftIO $ debugM logger ("DEBUG: " <> msg)

logNotice :: String -> ReaderT Config IO ()
logNotice msg = do
    Config { logger } <- ask
    liftIO $ noticeM logger msg

logError :: String -> ReaderT Config IO ()
logError msg = do
    Config { logger } <- ask
    liftIO $ criticalM logger msg
    liftIO $ exitFailure
