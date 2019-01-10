module Ruby
    ( install
    , installRbenvPlugin
    , compileRbenv
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
import qualified Symlinks

data RubyVersion
    = SelectedVersion String
    | NonSelectedVersion String
  deriving (Show)


compileRbenv = do
    Config { installationsDir, binDir } <- ask
    runProcess'
        $  "cd "
        <> installationsDir
        <> "/rbenv/rbenv && src/configure && make -C src"
    Symlinks.createSymlink (installationsDir <> "/rbenv/rbenv/bin/rbenv")
                           (binDir <> "/rbenv")


installRbenvPlugin name = do
    Config { installationsDir } <- ask
    Symlinks.createSymlink
        (installationsDir <> "/" <> name)
        (installationsDir <> "/rbenv/rbenv/plugins/ruby-build")


install :: String -> ReaderT Config IO ()
install version = do
    output <- runProcess "rbenv versions"
    case parseRubyVersions output of
        Left  err    -> logError (show err)
        Right result -> setGlobalVersion version result


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
