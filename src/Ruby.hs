module Ruby
    ( install
    )
where

import           System.Log.Logger
import           System.IO
import           Control.Monad
import           Process
import           Text.Parsec

data RubyVersion
    = SelectedVersion String
    | NonSelectedVersion String
  deriving (Show)


globalRubyVersion = "2.5.1"

logger = "BasicLogger"

install = do
    updateGlobalLogger logger $ setLevel NOTICE
    installRuby


installRuby = do
    rbenvInit
    output <- runProcess "rbenv versions" []
    case parseRubyVersions output of
        Left  err    -> putStrLn (show err)
        Right result -> setGlobalVersion globalRubyVersion result


rbenvInit = do
    noticeM logger "Initializing rbenv..."
  -- `rbenv init` has an error exit code for some reason even when it works
    runProcessNonStrict "rbenv init" []


setGlobalVersion version installedVersions = do
    unless requestedVersionExists $ installRubyVersion version
    useRubyVersion version
  where
    requestedVersionExists = any isRequestedVersion installedVersions
    isRequestedVersion installedVersion = case installedVersion of
        SelectedVersion    v -> v == version
        NonSelectedVersion v -> v == version


useRubyVersion version = do
    runProcess ("rbenv global " <> version) []
    noticeM logger $ "Global rbenv version set to " <> version


installRubyVersion version = do
    noticeM logger $ "Installing Ruby " <> version <> "..."
    runProcess ("rbenv install " <> version) []
    noticeM logger "Installation complete!"


parseRubyVersions output = parse parser "" (output :: String)
  where
    parser           = lineParser `sepEndBy` newline
    lineParser       = selectedVersion <|> availableVersion
    selectedVersion  = SelectedVersion <$> (char '*' *> rubyVersion)
    availableVersion = NonSelectedVersion <$> rubyVersion
    rubyVersion      = spaces *> (many1 $ noneOf "\n ")
