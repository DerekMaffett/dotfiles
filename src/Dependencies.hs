module Dependencies
  ( installDependencies
  )
where

import           System.Process
import qualified System.Directory              as Dir
import           System.Environment             ( lookupEnv )
import           Control.Monad
import           Data.Semigroup                 ( (<>) )

append = flip (<>)

getRelativePath :: String -> IO String
getRelativePath dir = (append $ "/" <> dir) <$> Dir.getHomeDirectory

unlessExists dir action = do
  exists <- Dir.doesPathExist dir
  unless exists action

--------------------------------------------------

brewPrograms =
  [ "autojump"
  , "neovim"
  , "cloc"
  , "tmux"
  , "the_silver_searcher"
  , "python"
  , "rbenv"
  ]

gems = ["tmuxinator"]

stackPrograms = ["brittany"]

globalNpmPackages = ["elm", "elm-format"]

data PluginType = Theme

data ZshPlugin = ZshPlugin { author :: String
                           , name :: String
                           , pluginType :: PluginType
                           }

zshPlugins = [ZshPlugin "bhilburn" "powerlevel9k" Theme]

--------------------------------------------------

brewInstall package = callCommand $ "brew install " <> package

npmInstall package =
  callCommand $ "source ~/.nvm/nvm.sh && npm install -g " <> package

stackInstall package = callCommand $ "stack install " <> package

gemInstall package =
  callCommand $ "eval \"$(rbenv init -)\" && gem install " <> package

zshInstall package = do
  path <- getRelativePath $ packagePath
  unlessExists path
    $  callCommand
    $  "git clone https://github.com/"
    <> author package
    <> "/"
    <> name package
    <> ".git ~/"
    <> packagePath
 where
  packagePath = ".oh-my-zsh/custom/" <> installLocation <> "/" <> name package
  installLocation = case pluginType package of
    Theme -> "themes"

--------------------------------------------------

installBrewDependencies = mapM_ brewInstall brewPrograms
installStackDependencies = mapM_ stackInstall stackPrograms
installOhMyZshPlugins = mapM_ zshInstall zshPlugins
installNpmPackages = mapM_ npmInstall globalNpmPackages
installGems = mapM_ gemInstall gems

installVimPlug =
  callCommand
    "curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"

installDeopleteDependency = callCommand "pip3 install --user pynvim"

installNVM = mapM_
  callCommand
  [ "curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.11/install.sh | zsh"
  , "source ~/.nvm/nvm.sh && nvm install v11.2.0"
  ]

installZsh = do
  maybeShellVar <- lookupEnv "SHELL"
  command maybeShellVar
 where
  setShell = callCommand "chsh -s $(which zsh)"
  command maybeShellVar = case maybeShellVar of
    Nothing       -> setShell
    Just shellVar -> when (shellVar /= "/bin/zsh") setShell


installOhMyZsh = do
  path <- getRelativePath ".oh-my-zsh"
  unlessExists path $ callCommand
    "git clone https://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh"

installPowerlineFonts = do
  path <- getRelativePath ".powerline-fonts"
  unlessExists path $ mapM_
    callCommand
    [ "git clone https://github.com/powerline/fonts.git --depth=1 ~/.powerline-fonts"
    , "~/.powerline-fonts/install.sh"
    ]

-- `rbenv init` has an error exit code for some reason even when it works
installRuby =
  callCommand "rbenv init || rbenv install 2.6.0 && rbenv global 2.6.0"

installTerminalColors = do
  path <- getRelativePath "iterm-colors"
  unlessExists path
    $ callCommand
        "git clone git@github.com:mbadolato/iTerm2-Color-Schemes.git ~/iterm-colors"

installTmuxinatorCompletions = do
  path <- getRelativePath ".tmuxinator"
  unlessExists path $ callCommand
    "git clone git@github.com:tmuxinator/tmuxinator.git ~/.tmuxinator"

installDependencies = do
  installBrewDependencies
  installStackDependencies
  installRuby
  installGems
  installNVM
  installNpmPackages
  installTmuxinatorCompletions
  installZsh
  installOhMyZsh
  installOhMyZshPlugins
  installPowerlineFonts
  installTerminalColors
  installVimPlug
  installDeopleteDependency
