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
-- import           Control.Monad.Reader
import qualified Ruby

-- getRelativePath :: String -> IO String
-- getRelativePath dir = (append $ "/" <> dir) <$> Dir.getHomeDirectory
--
-- unlessExists dir action = do
--     exists <- Dir.doesPathExist dir
--     unless exists action

--------------------------------------------------

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
-- installPowerlineFonts = do
--     path <- getRelativePath ".powerline-fonts"
--     unlessExists path $ mapM_
--         callCommand
--         [ "git clone https://github.com/powerline/fonts.git --depth=1 ~/.powerline-fonts"
--         , "~/.powerline-fonts/install.sh"
--         ]
--
logSection action = logNotice "" >> action >> logNotice ""
--
-- installTerminalColors = do
--     path <- getRelativePath "iterm-colors"
--     unlessExists path
--         $ callCommand
--               "git clone git@github.com:mbadolato/iTerm2-Color-Schemes.git ~/iterm-colors"

installTmuxinatorCompletions = do
    logNotice "Installing tmuxinator completions..."
    runProcess "rm -rf ~/.tmuxinator" []
    runProcess
        "git clone git@github.com:tmuxinator/tmuxinator.git ~/.tmuxinator"
        []

install = do
    logSection Homebrew.install
    logSection Ruby.install
    logSection Node.install
    Stack.installPackages
    Ruby.installPackages
    Node.installPackages
    installTmuxinatorCompletions
    Zsh.install
    return ()
  -- installPowerlineFonts
  -- installTerminalColors
  -- installVimPlug
  -- installDeopleteDependency
