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



installVimPlug = do
    logNotice "Installing vim plug..."
    runProcess
        "curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
        []


pythonPackages = ["pynvim"]


installPythonPackages = mapM_ pip3Install pythonPackages


pip3Install package = runProcess ("pip3 install --user " <> package) []


logSection action = logNotice "" >> action >> logNotice ""

initInstallationsDir = do
    Config { installationsDir } <- ask
    runProcess ("rm -rf " <> installationsDir) []
    runProcess ("mkdir " <> installationsDir)  []

installTmuxinatorCompletions = do
    Config { installationsDir } <- ask
    logNotice "Installing tmuxinator completions..."
    logDebug "You'll need to link to tmuxinator completions in your zshrc file!"
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
    installPythonPackages
    Stack.installPackages
    Ruby.installPackages
    Node.installPackages
    installTmuxinatorCompletions
    Zsh.install
    TerminalHappiness.install
    installVimPlug
    return ()
