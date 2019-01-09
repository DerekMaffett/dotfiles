module Packages
    ( install
    )
where

import           Process
import           Config
import           Logger
import           Safe                           ( headMay )
import           Data.List
import           Control.Monad
import           Control.Monad.Reader
import           Data.Maybe
import qualified Symlinks
import qualified Ruby
import qualified Node
import qualified Zsh

data ZshPluginType = Theme | Plugin

data Source
  = Ruby
  | Brew
  | Stack
  | Python
  | Npm
  | Github String
  | Zsh ZshPluginType String
  | Custom (ReaderT Config IO ())


data Package
  = Package
  { name :: String
  , source :: Source
  }


preSources = [Package {name = "rbenv", source = Brew}]

sources =
    [ Package {name = "ruby", source = Custom (Ruby.install "2.6.0")}
    , Package {name = "node", source = Custom Node.install}
    , Package {name = "python", source = Brew}
    , Package {name = "oh-my-zsh", source = Github "robbyrussell/oh-my-zsh"}
    ]

packages =
    [ Package {name = "pynvim", source = Python}
    , Package {name = "tmuxinator", source = Ruby}
    , Package {name = "brittany", source = Stack}
    , Package {name = "elm", source = Npm}
    , Package {name = "elm-format", source = Npm}
    , Package {name = "autojump", source = Brew}
    , Package {name = "neovim", source = Brew}
    , Package {name = "cloc", source = Brew}
    , Package {name = "tmux", source = Brew}
    , Package {name = "the_silver_searcher", source = Brew}
    , Package {name = "vim-plug", source = Custom installVimPlug}
    , Package {name = "tmuxinator", source = Github "tmuxinator/tmuxinator"}
    , Package {name = "zsh", source = Custom Zsh.setShell}
    , Package {name = "powerlevel9k", source = Zsh Theme "bhilburn"}
    , Package {name = "zsh-completions", source = Zsh Plugin "zsh-users"}
    ]

installVimPlug =
    runProcess'
        "curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"


zshInstall pluginType author name = do
    Config { installationsDir } <- ask
    _githubInstall (packagePath installationsDir) (author <> "/" <> name) name
  where
    packagePath installationsDir =
        installationsDir
            <> "/oh-my-zsh/custom/"
            <> pluginLocation
            <> "/"
            <> name
    pluginLocation = case pluginType of
        Theme  -> "themes"
        Plugin -> "plugins"

githubInstall gitAddress name = do
    Config { installationsDir } <- ask
    _githubInstall (installationsDir <> "/" <> name) gitAddress name

_githubInstall targetPath gitAddress name = runProcess'
    ("git clone git@github.com:" <> gitAddress <> ".git " <> targetPath)

brewUpgrade name = do
    logNotice $ "Upgrading " <> name
    runProcess ("brew upgrade " <> name)

stackInstall name = runProcess' ("stack install " <> name)

pip3Install name = runProcess' ("pip3 install --user " <> name)

gemInstall name =
    runProcess' ("eval \"$(rbenv init -)\" && gem install " <> name)

npmInstall name =
    runProcess' ("source ~/.nvm/nvm.sh && npm install -g " <> name)

brewInstall name = do
    isUninstalled <- checkIfInstalled <$> runProcess "brew list"
    runProcess' ("brew install " <> name)
    where checkIfInstalled name = (\installed -> name `elem` installed) . words

installPackage Package { name, source } = do
    logNotice $ "Installing " <> name <> "..."
    case source of
        Python                    -> pip3Install name
        Ruby                      -> gemInstall name
        Stack                     -> stackInstall name
        Npm                       -> npmInstall name
        Brew                      -> brewInstall name
        Github gitAddress         -> githubInstall gitAddress name
        Zsh pluginType author     -> zshInstall pluginType author name
        Custom installationMethod -> installationMethod

updateBrewPackages = do
    outdatedPackages <- getOutdated <$> runProcess "brew outdated"
    logDebug $ "Outdated packages: " <> show outdatedPackages
    mapM_ brewUpgrade outdatedPackages
    where getOutdated = catMaybes . (map headMay) . (map words) . lines

install = do
    logNotice "Updating Homebrew..."
    runProcess "brew update"
    updateBrewPackages
    mapM_ installPackage preSources
    mapM_ installPackage sources
    mapM_ installPackage packages
