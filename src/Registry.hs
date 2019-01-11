module Registry
    ( registryLookup
    , registryMember
    , Package(..)
    , Source(..)
    , GitAddress(..)
    , ZshPluginType(..)
    , toString
    )
where

import qualified Data.HashMap.Strict           as HashMap
import           Config
import           Control.Monad.Reader
import           Safe                           ( headMay )
import           Process
import           Logger
import           Data.Maybe
import qualified Node
import qualified Ruby
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
  | Batch [Source]
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


registry :: HashMap.HashMap String Package
registry =
    (HashMap.fromList)
        . fmap (\package -> ((name :: Package -> String) package, package))
        $ [ Package
              { name   = "brew"
              , source = Batch
                  [ Custom $ runProcess' "brew update"
                  , Custom updateBrewPackages
                  ]
              }
          , Package
              { name   = "neovim"
              , source = Batch
                  [ withBranch "release-0.3" $ github "neovim" "neovim"
                  , Custom Vim.make
                  ]
              }
          , Package
              { name   = "rbenv"
              , source = Batch
                  [ github "rbenv" "rbenv"
                  , github "rbenv" "ruby-build"
                  , Custom $ Ruby.compileRbenv
                  , Custom $ Ruby.installRbenvPlugin "rbenv/ruby-build"
                  ]
              }
          , Package {name = "ruby", source = Custom (Ruby.install "2.6.0")}
          , Package {name = "node", source = Custom Node.install}
          , brew "python"
          , brew "ninja"
          , brew "libtool"
          , brew "automake"
          , brew "cmake"
          , brew "pkg-config"
          , brew "gettext"
          , Package
              { name   = "oh-my-zsh"
              , source = github "robbyrussell" "oh-my-zsh"
              }
          , Package {name = "pynvim", source = Python "pynvim"}
          , Package {name = "tmuxinator", source = Ruby "tmuxinator"}
          , Package {name = "elm-test", source = Npm "elm-test"}
          , Package {name = "brittany", source = Stack "brittany"}
          , Package {name = "elm", source = Npm "elm"}
          , Package {name = "elm-format", source = Npm "elm-format"}
          , brew "autojump"
          , Package {name = "cloc", source = Npm "cloc"}
          , brew "tmux"
          , brew "the_silver_searcher"
          , Package {name = "vim-plug", source = Custom installVimPlug}
          , Package
              { name   = "tmuxinator"
              , source = github "tmuxinator" "tmuxinator"
              }
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
              , source = Batch
                  [github "powerline" "fonts", Custom installPowerlineFonts]
              }
          , Package
              { name   = "iTerm2-color-schemes"
              , source = github "mbadolato" "iTerm2-Color-Schemes"
              }
          ]

registryLookup packageName = HashMap.lookup packageName registry
registryMember packageName = HashMap.member packageName registry

installPowerlineFonts = do
    Config { installationsDir } <- ask
    runProcess' (installationsDir <> "/powerline/fonts/install.sh")


installVimPlug =
    runProcess'
        "curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"

brewUpgrade name = do
    logNotice $ "Upgrading " <> name
    runProcess ("brew upgrade " <> name)

updateBrewPackages = do
    outdatedPackages <- getOutdated <$> runProcess "brew outdated"
    logDebug $ "Outdated packages: " <> show outdatedPackages
    mapM_ brewUpgrade outdatedPackages
    where getOutdated = catMaybes . (map headMay) . (map words) . lines
