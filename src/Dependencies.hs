module Dependencies
  ( installDependencies
  )
where

import           System.Process
import qualified System.Directory              as Dir
import           Control.Monad
import           Data.Semigroup                 ( (<>) )

brewPrograms =
  ["autojump", "neovim", "cloc", "tmux", "the_silver_searcher", "python"]

stackPrograms = ["brittany"]

globalNpmPackages = ["elm-format"]

append = flip (<>)

getRelativePath :: String -> IO String
getRelativePath dir = (append $ "/" <> dir) <$> Dir.getHomeDirectory

unlessExists dir action = do
  exists <- Dir.doesPathExist dir
  unless exists action

brewInstall package = callCommand $ "brew install " <> package

npmInstall package =
  callCommand $ "source ~/.nvm/nvm.sh && npm install -g " <> package

stackInstall package = callCommand $ "stack install " <> package

installBrewDependencies = mapM_ brewInstall brewPrograms
installStackDependencies = mapM_ stackInstall stackPrograms

installVimPlug =
  callCommand
    "curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"

installDeopleteDependency = callCommand "pip3 install --user pynvim"

installNVM = mapM_
  callCommand
  [ "curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.11/install.sh | zsh"
  , "source ~/.nvm/nvm.sh && nvm install v11.2.0"
  ]

installZsh = callCommand "chsh -s $(which zsh)"

installOhMyZsh = do
  path <- getRelativePath ".oh-my-zsh"
  unlessExists path
    $ mapM_ callCommand
    $ ["git clone https://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh"]

installPowerlineFonts = do
  path <- getRelativePath ".powerline-fonts"
  unlessExists path $ mapM_
    callCommand
    [ "git clone https://github.com/powerline/fonts.git --depth=1 ~/.powerline-fonts"
    , "~/.powerline-fonts/install.sh"
    ]

installNpmPackages = mapM_ npmInstall globalNpmPackages

installDependencies = do
  installBrewDependencies
  installStackDependencies
  installNVM
  installNpmPackages
  installZsh
  installOhMyZsh
  installPowerlineFonts
  installVimPlug
  installDeopleteDependency
