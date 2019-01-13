module Registry
    ( registryLookup
    , registryMember
    , Registry
    , Package(..)
    , Source(..)
    , GitAddress(..)
    , ZshPluginType(..)
    , toString
    , centralRegistry
    , createRegistry
    )
where

import qualified Data.HashMap.Strict           as HashMap
import           Config
import           Control.Monad.Reader
import           Safe                           ( headMay )
import           Process
import           Logger
import           Data.Maybe
import qualified Registry.Node                 as Node
import qualified Registry.Ruby                 as Ruby
import qualified Registry.Zsh                  as Zsh
import qualified Registry.Vim                  as Vim


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
  , dependencies :: [Package]
  , configs :: [Config]
  }

-- Package creators
githubAddress author name =
    GitAddress {author = author, name = name, branch = "master"}

withBranch overrideBranch (GitAddress { author, name, branch }) =
    GitAddress {author = author, name = name, branch = overrideBranch}

zshTheme name address = Package
    { name         = name
    , source       = Zsh Theme address
    , dependencies = []
    , configs      = []
    }

zshPlugin name address = Package
    { name         = name
    , source       = Zsh Plugin address
    , dependencies = []
    , configs      = []
    }

rubyPackage name = Package
    { name         = name
    , source       = Ruby name
    , dependencies = [ruby]
    , configs      = []
    }

brewPackage name = Package
    { name         = name
    , source       = Brew name
    , dependencies = [homebrew]
    , configs      = []
    }

pythonPackage name = Package
    { name         = name
    , source       = Python name
    , dependencies = [python]
    , configs      = []
    }

npmPackage name = Package
    { name         = name
    , source       = Npm name
    , dependencies = [node]
    , configs      = []
    }

githubPackage name address = Package
    { name         = name
    , source       = Github address
    , dependencies = []
    , configs      = []
    }

-- TODO: add stack as dependency
stackPackage name =
    Package {name = name, source = Stack name, dependencies = [], configs = []}

withDependencies additionalDependencies Package { name, source, dependencies }
    = Package
        { name         = name
        , source       = source
        , dependencies = dependencies <> additionalDependencies
        , configs      = []
        }


-- Common packages
homebrew = Package
    { name         = "brew"
    , source       = Batch
        [Custom $ runProcess' "brew update", Custom updateBrewPackages]
    , dependencies = []
    , configs      = []
    }

rbenv = Package
    { name         = "rbenv"
    , source       = Batch
        [ Github $ githubAddress "rbenv" "rbenv"
        , Github $ githubAddress "rbenv" "ruby-build"
        , Custom $ Ruby.compileRbenv
        , Custom $ Ruby.installRbenvPlugin "rbenv/ruby-build"
        ]
    , dependencies = []
    , configs      = []
    }

ruby = Package
    { name         = "ruby"
    , source       = Custom (Ruby.install "2.6.0")
    , dependencies = [rbenv]
    , configs      = []
    }

python = brewPackage "python"

neovim = Package
    { name         = "neovim"
    , source       = Batch
        [ Github $ withBranch "release-0.3" $ githubAddress "neovim" "neovim"
        , Custom Vim.make
        ]
    , dependencies = [ brewPackage "ninja"
                     , brewPackage "libtool"
                     , brewPackage "automake"
                     , brewPackage "cmake"
                     , brewPackage "pkg-config"
                     , brewPackage "gettext"
                     -- TODO: pynvim is really for deoplete, but we can't
                     -- currently express that dependency relationship
                     , pythonPackage "pynvim"
                     ]
    , configs      = []
    }


tmuxinator =
    withDependencies
            [ githubPackage "tmuxinator-completions"
                  $ githubAddress "tmuxinator" "tmuxinator"
            ]
        $ rubyPackage "tmuxinator"

node = Package
    { name         = "node"
    , source       = Custom Node.install
    , dependencies = []
    , configs      = []
    }

-- Registry

type Registry = HashMap.HashMap String Package

createRegistry :: [Package] -> Registry
createRegistry = (HashMap.fromList)
    . fmap (\package -> ((name :: Package -> String) package, package))

centralRegistry :: Registry
centralRegistry = createRegistry
    [ neovim
    , tmuxinator
    , brewPackage "autojump"
    , brewPackage "tmux"
    , brewPackage "the_silver_searcher"
    , stackPackage "brittany"
    , npmPackage "elm-test"
    , npmPackage "elm"
    , npmPackage "elm-format"
    , npmPackage "cloc"
    , npmPackage "prettier"
    , githubPackage "oh-my-zsh" $ githubAddress "robbyrussell" "oh-my-zsh"
    , githubPackage "iTerm2-color-schemes"
        $ githubAddress "mbadolato" "iTerm2-Color-Schemes"
    , Package
        { name         = "vim-plug"
        , source       = Custom installVimPlug
        , dependencies = [neovim]
        , configs      = []
        }
    , Package
        { name         = "zsh"
        , source       = Custom Zsh.setShell
        , dependencies = [ zshPlugin "zsh-completions"
                               $ githubAddress "zsh-users" "zsh-completions"
                         ]
        , configs      = []
        }
    , zshTheme "powerlevel9k" $ githubAddress "bhilburn" "powerlevel9k"
    , Package
        { name         = "powerline-fonts"
        , source       = Batch
            [ Github $ githubAddress "powerline" "fonts"
            , Custom installPowerlineFonts
            ]
        , dependencies = []
        , configs      = []
        }
    ]

registryLookup registry packageName = HashMap.lookup packageName registry
registryMember registry packageName = HashMap.member packageName registry

installPowerlineFonts = do
    Config { installationsDir } <- ask
    runProcess' (installationsDir <> "/powerline/fonts/install.sh")


installVimPlug =
    runProcess'
        "curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"

brewUpgrade name = runProcess ("brew upgrade " <> name)

updateBrewPackages = do
    outdatedPackages <- getOutdated <$> runProcess "brew outdated"
    logDebug $ "Outdated packages: " <> show outdatedPackages
    mapM_ brewUpgrade outdatedPackages
    where getOutdated = catMaybes . (map headMay) . (map words) . lines
