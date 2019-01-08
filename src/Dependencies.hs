module Dependencies
    ( install
    )
where

import qualified System.Directory              as Dir
import           System.Environment             ( lookupEnv )
import           Control.Monad
import qualified Homebrew
import           Config
import qualified Node
import qualified Stack
import           Process
import qualified Zsh
import           Logger
import qualified TerminalHappiness
import           Control.Monad.Reader
import qualified Ruby


--
-- installVimPlug =
--     callCommand
--         "curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
--
-- installDeopleteDependency = callCommand "pip3 install --user pynvim"
--
--
--
--
--
logSection action = logNotice "" >> action >> logNotice ""

initInstallationsDir = do
    Config { installationsDir } <- ask
    runProcess ("rm -rf " <> installationsDir) []
    runProcess ("mkdir " <> installationsDir)  []

installTmuxinatorCompletions = do
    Config { installationsDir } <- ask
    logNotice "Installing tmuxinator completions..."
    runProcess
        (  "git clone git@github.com:tmuxinator/tmuxinator.git "
        <> installationsDir
        <> "/tmuxinator"
        )
        []

install = do
    initInstallationsDir
    logSection Homebrew.install
    logSection Ruby.install
    logSection Node.install
    Stack.installPackages
    Ruby.installPackages
    Node.installPackages
    installTmuxinatorCompletions
    Zsh.install
    TerminalHappiness.install
    return ()
  -- installVimPlug
  -- installDeopleteDependency
