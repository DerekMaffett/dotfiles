module Dependencies
  ( installDependencies
  )
where

import           System.Process
import qualified System.Directory              as Dir
import           Control.Monad
import           Data.Semigroup                 ( (<>) )

brewPrograms =
  ["jump", "neovim", "cloc", "tmux", "the_silver_searcher", "python"]

stackPrograms = ["brittany"]

globalNpmPackages = ["elm-format"]

append = flip (<>)

getRelativePath :: String -> IO String
getRelativePath dir = (append $ "/" <> dir) <$> Dir.getHomeDirectory

unlessExists dir action = do
  exists <- Dir.doesPathExist dir
  unless exists action

brewInstall package = callCommand $ "brew install " <> package
npmInstall package = callCommand $ "npm install -g " <> package
stackInstall package = callCommand $ "stack install " <> package

installBrewDependencies = sequence_ $ map brewInstall brewPrograms
installStackDependencies = mapM_ stackInstall stackPrograms

installVimPlug = do
  callCommand
    "curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
  callCommand "vim +PlugInstall +PlugClean! +qall"

installDeopleteDependency = callCommand "pip3 install --user pynvim"

installNVM = mapM_
  callCommand
  [ "curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.11/install.sh | bash"
  , "source ~/.nvm/nvm.sh && nvm install v11.2.0"
  ]

installBashGitPrompt = do
  path <- getRelativePath ".bash-git-prompt"
  unlessExists path $ callCommand
    (  "git clone https://github.com/magicmonty/bash-git-prompt.git "
    <> path
    <> " --depth=1"
    )

installNpmPackages = mapM_ callCommand globalNpmPackages

installDependencies = do
  installBrewDependencies
  installStackDependencies
  installNVM
  installNpmPackages
  installBashGitPrompt
  installVimPlug
  installDeopleteDependency
