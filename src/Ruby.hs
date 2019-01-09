module Ruby
    ( install
    )
where

import           System.Log.Logger
import           System.IO
import           Control.Monad.Reader
import           Control.Monad
import           Config
import           Process
import           Logger
import           Text.Parsec

data RubyVersion
    = SelectedVersion String
    | NonSelectedVersion String
  deriving (Show)



install :: String -> ReaderT Config IO ()
install = installRuby


installRuby :: String -> ReaderT Config IO ()
installRuby version = do
    rbenvInit
    output <- runProcess "rbenv versions"
    case parseRubyVersions output of
        Left  err    -> logError (show err)
        Right result -> setGlobalVersion version result


rbenvInit = do
    logNotice "Initializing rbenv..."
  -- `rbenv init` has an error exit code for some reason even when it works
    runProcessNonStrict "rbenv init"


setGlobalVersion :: String -> [RubyVersion] -> ReaderT Config IO ()
setGlobalVersion version installedVersions = do
    unless requestedVersionExists $ installRubyVersion version
    useRubyVersion version
  where
    requestedVersionExists = any isRequestedVersion installedVersions
    isRequestedVersion installedVersion = case installedVersion of
        SelectedVersion    v -> v == version
        NonSelectedVersion v -> v == version


useRubyVersion :: String -> ReaderT Config IO ()
useRubyVersion version = do
    runProcess ("rbenv global " <> version)
    logNotice $ "Global rbenv version set to " <> version


installRubyVersion :: String -> ReaderT Config IO ()
installRubyVersion version = do
    logNotice ("Installing Ruby " <> version <> "...")
    runProcess ("rbenv install " <> version)
    logNotice "Installation complete!"


parseRubyVersions output = parse parser "" (output :: String)
  where
    parser           = lineParser `sepEndBy` newline
    lineParser       = selectedVersion <|> availableVersion
    selectedVersion  = SelectedVersion <$> (char '*' *> rubyVersion)
    availableVersion = NonSelectedVersion <$> rubyVersion
    rubyVersion      = spaces *> (many1 $ noneOf "\n ")
