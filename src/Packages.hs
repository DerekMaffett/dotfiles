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
import qualified Vim

data ZshPluginType = Theme | Plugin

data GitAddress =
  GitAddress
  { author :: String
  , name :: String
  , branch :: String
  }

toString GitAddress { author, name } = author <> "/" <> name

data Source
  = Ruby String
  | Brew String
  | Stack String
  | Python String
  | Npm String
  | Github GitAddress
  | Zsh ZshPluginType GitAddress
  | Custom (ReaderT Config IO ())


data Package
  = Package
  { name :: String
  , source :: Source
  }

github author name =
    Github $ GitAddress {author = author, name = name, branch = "master"}

withBranch overrideBranch (Github GitAddress { author, name, branch }) =
    Github $ GitAddress {author = author, name = name, branch = overrideBranch}

zshTheme author name =
    Zsh Theme $ GitAddress {author = author, name = name, branch = "master"}

zshPlugin author name =
    Zsh Plugin $ GitAddress {author = author, name = name, branch = "master"}

brew name = Package {name = name, source = Brew name}

preSources
    = [ Package
          { name   = "neovim"
          , source = batch
              [ withBranch "release-0.3" $ github "neovim" "neovim"
              , Custom Vim.make
              ]
          }
      , Package
          { name   = "rbenv"
          , source = batch
              [ github "rbenv" "rbenv"
              , github "rbenv" "ruby-build"
              , Custom $ Ruby.compileRbenv
              , Custom $ Ruby.installRbenvPlugin "rbenv/ruby-build"
              ]
          }
      ]

sources =
    [ Package {name = "ruby", source = Custom (Ruby.install "2.6.0")}
    , Package {name = "node", source = Custom Node.install}
    , brew "python"
    , Package {name = "oh-my-zsh", source = github "robbyrussell" "oh-my-zsh"}
    , brew "ninja"
    , brew "libtool"
    , brew "automake"
    , brew "cmake"
    , brew "pkg-config"
    , brew "gettext"
    ]

packages
    = [ Package {name = "pynvim", source = Python "pynvim"}
      , Package {name = "tmuxinator", source = Ruby "tmuxinator"}
      , Package {name = "brittany", source = Stack "brittany"}
      , Package {name = "elm", source = Npm "elm"}
      , Package {name = "elm-format", source = Npm "elm-format"}
      , brew "autojump"
      , Package {name = "cloc", source = Npm "cloc"}
      , brew "tmux"
      , brew "the_silver_searcher"
      , Package {name = "vim-plug", source = Custom installVimPlug}
      , Package {name = "tmuxinator", source = github "tmuxinator" "tmuxinator"}
      , Package {name = "zsh", source = Custom Zsh.setShell}
      , Package
          { name   = "powerlevel9k"
          , source = zshTheme "bhilburn" "powerlevel9k"
          }
      , Package
          { name   = "zsh-completions"
          , source = zshPlugin "zsh-users" "zsh-completions"
          }
      , Package {name = "prettier", source = Npm "prettier"}
      , Package
          { name   = "powerline-fonts"
          , source = batch
              [github "powerline" "fonts", Custom installPowerlineFonts]
          }
      , Package
          { name   = "iTerm2-color-schemes"
          , source = github "mbadolato" "iTerm2-Color-Schemes"
          }
      ]

batch installationSteps = Custom $ mapM_ installFromSource installationSteps

installPowerlineFonts = do
    Config { installationsDir } <- ask
    runProcess' (installationsDir <> "/powerline/fonts/install.sh")


installVimPlug =
    runProcess'
        "curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"


zshInstall pluginType gitAddress = do
    Config { installationsDir } <- ask
    _githubInstall (packagePath installationsDir) gitAddress
  where
    packagePath installationsDir =
        installationsDir
            <> "/robbyrussell/oh-my-zsh/custom/"
            <> pluginLocation
            <> "/"
            <> (name :: GitAddress -> String) gitAddress
    pluginLocation = case pluginType of
        Theme  -> "themes"
        Plugin -> "plugins"

githubInstall gitAddress = do
    Config { installationsDir } <- ask
    _githubInstall (installationsDir <> "/" <> toString gitAddress) gitAddress

_githubInstall targetPath gitAddress = runProcess'
    (  "git clone --single-branch --branch "
    <> (branch :: GitAddress -> String) gitAddress
    <> " git@github.com:"
    <> toString gitAddress
    <> ".git "
    <> targetPath
    )

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

installFromSource source = case source of
    Python name               -> pip3Install name
    Ruby   name               -> gemInstall name
    Stack  name               -> stackInstall name
    Npm    name               -> npmInstall name
    Brew   name               -> brewInstall name
    Github gitAddress         -> githubInstall gitAddress
    Zsh pluginType gitAddress -> zshInstall pluginType gitAddress
    Custom installationMethod -> installationMethod

installPackage Package { name, source } = do
    logNotice $ "Installing " <> name <> "..."
    installFromSource source

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
