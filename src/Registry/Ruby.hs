module Registry.Ruby
    ( install
    , installRbenv
    , rbenvCommand
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


rbenvCommand shellCommand =
    "RBENV_ROOT=~/dotfiles/.devfiles/.installations/rbenv/rbenv "
        <> "PATH=~/dotfiles/.devfiles/.installations/rbenv/rbenv/shims:$PATH && "
        <> shellCommand


installRbenv = do
    redirectRbenv
    compileRbenv
    installRbenvPlugin "rbenv/ruby-build"


redirectRbenv = do
    Config { homeDir, installationsDir } <- ask
    Symlinks.createSymlink (installationsDir <> "/rbenv/rbenv")
                           (homeDir <> "/.rbenv")


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
install version = setGlobalVersion version


setGlobalVersion :: String -> ReaderT Config IO ()
setGlobalVersion version = do
    installRubyVersion version
    useRubyVersion version


useRubyVersion :: String -> ReaderT Config IO ()
useRubyVersion version = do
    runProcess $ rbenvCommand ("rbenv global " <> version)
    logNotice $ "Global rbenv version set to " <> version


installRubyVersion :: String -> ReaderT Config IO ()
installRubyVersion version = do
    logNotice ("Installing Ruby " <> version <> "...")
    runProcess $ rbenvCommand ("rbenv install " <> version)
    logNotice "Installation complete!"

-- Unused currently since versions are no longer queried
parseRubyVersions output = parse parser "" (output :: String)
  where
    parser           = lineParser `sepEndBy` newline
    lineParser       = selectedVersion <|> availableVersion
    selectedVersion  = SelectedVersion <$> (char '*' *> rubyVersion)
    availableVersion = NonSelectedVersion <$> rubyVersion
    rubyVersion      = spaces *> (many1 $ noneOf "\n ")
