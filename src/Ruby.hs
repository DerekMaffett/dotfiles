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
import           Text.Parsec

data RubyVersion
    = SelectedVersion String
    | NonSelectedVersion String
  deriving (Show)


globalRubyVersion = "2.6.0"

install :: ReaderT Config IO ()
install = do
    installRuby


installRuby :: ReaderT Config IO ()
installRuby = do
    rbenvInit
    output <- liftIO $ runProcess "rbenv versions" []
    case parseRubyVersions output of
        Left  err    -> liftIO $ putStrLn (show err)
        Right result -> setGlobalVersion globalRubyVersion result


rbenvInit = do
    Config { logger } <- ask
    liftIO $ noticeM logger "Initializing rbenv..."
  -- `rbenv init` has an error exit code for some reason even when it works
    liftIO $ runProcessNonStrict "rbenv init" []


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
    Config { logger } <- ask
    liftIO $ runProcess ("rbenv global " <> version) []
    liftIO $ (noticeM logger $ "Global rbenv version set to " <> version)


installRubyVersion :: String -> ReaderT Config IO ()
installRubyVersion version = do
    Config { logger } <- ask
    liftIO $ noticeM logger ("Installing Ruby " <> version <> "...")
    liftIO $ runProcess ("rbenv install " <> version) []
    liftIO $ noticeM logger "Installation complete!"


parseRubyVersions output = parse parser "" (output :: String)
  where
    parser           = lineParser `sepEndBy` newline
    lineParser       = selectedVersion <|> availableVersion
    selectedVersion  = SelectedVersion <$> (char '*' *> rubyVersion)
    availableVersion = NonSelectedVersion <$> rubyVersion
    rubyVersion      = spaces *> (many1 $ noneOf "\n ")
