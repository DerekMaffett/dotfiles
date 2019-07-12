module Registry
    ( registryLookup
    , registryMember
    , Registry
    , Package(..)
    , Snippet(..)
    , Source(..)
    , PackageConfig(..)
    , SymlinkTarget(..)
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
  | BrewCask String
  | Apt String
  | Stack String
  | Python String
  | Npm String
  | Github GitAddress
  | Zsh ZshPluginType GitAddress
  | Batch [Source]
  | Custom (ReaderT Config IO ())


data SymlinkTarget
  = Home
  | SshConfig
  | XDGConfig String String deriving (Show, Eq)

data PackageConfig
  =  PackageConfig String SymlinkTarget deriving (Show, Eq)

data Snippet = Snippet PackageConfig String deriving (Show, Eq)

data Package
  = Package
  { name :: String
  , source :: Source
  , dependencies :: [Package]
  , config :: Maybe PackageConfig
  , snippets :: [Snippet]
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
    , config       = Nothing
    , snippets     = []
    }

zshPlugin name address = Package
    { name         = name
    , source       = Zsh Plugin address
    , dependencies = []
    , config       = Nothing
    , snippets     = []
    }

rubyPackage name = Package
    { name         = name
    , source       = Ruby name
    , dependencies = [ruby]
    , config       = Nothing
    , snippets     = []
    }

brewPackage name = Package
    { name         = name
    , source       = Brew name
    , dependencies = [homebrew]
    , config       = Nothing
    , snippets     = []
    }

aptPackage name = Package
    { name         = name
    , source       = Apt name
    , dependencies = []
    , config       = Nothing
    , snippets     = []
    }

brewCaskPackage name = Package
    { name         = name
    , source       = BrewCask name
    , dependencies = [homebrew]
    , config       = Nothing
    , snippets     = []
    }

pythonPackage name = Package
    { name         = name
    , source       = Python name
    , dependencies = [python, pip3]
    , config       = Nothing
    , snippets     = []
    }

npmPackage name = Package
    { name         = name
    , source       = Npm name
    , dependencies = [node]
    , config       = Nothing
    , snippets     = []
    }

githubPackage name address = Package
    { name         = name
    , source       = Github address
    , dependencies = []
    , config       = Nothing
    , snippets     = []
    }

-- TODO: add stack as dependency
stackPackage name = Package
    { name         = name
    , source       = Stack name
    , dependencies = [stack]
    , config       = Nothing
    , snippets     = []
    }

withDependencies additionalDependencies Package { name, source, dependencies }
    = Package
        { name         = name
        , source       = source
        , dependencies = dependencies <> additionalDependencies
        , config       = Nothing
        , snippets     = []
        }


basicPackage name = Package
    { name         = name
    , source       = noopSource
    , dependencies = []
    , config       = Nothing
    , snippets     = []
    }


-- Common packages
homebrew = Package
    { name         = "brew"
    , source       = Batch
        [Custom $ runProcess' "brew update", Custom updateBrewPackages]
    , dependencies = []
    , config       = Nothing
    , snippets     = []
    }

curl = aptPackage "curl"

rbenv = Package
    { name         = "rbenv"
    , source       = Batch
        [ Github $ githubAddress "rbenv" "rbenv"
        , Github $ githubAddress "rbenv" "ruby-build"
        , Custom $ Ruby.installRbenv
        ]
    , dependencies = []
    , config       = Nothing
    , snippets     = [Snippet zshrc $ unlines ["eval \"$(rbenv init -)\""]]
    }

ruby = Package
    { name         = "ruby"
    , source       = Custom (Ruby.install "2.6.2")
    , dependencies = [ rbenv
                     , aptPackage "libssl-dev"
                     , aptPackage "libreadline-dev"
                     , aptPackage "zlib1g-dev"
                     ]
    , config       = Nothing
    , snippets     = []
    }

stack = (basicPackage "stack")
    { snippets = [ Snippet zshrc
                       $ unlines ["export PATH=$HOME/.local/bin:$PATH"]
                 ]
    }


python = aptPackage "python3.7"
pip3 = aptPackage "python3-pip"

tmuxinator = ( withDependencies
                     [ githubPackage "tmuxinator-completions"
                           $ githubAddress "tmuxinator" "tmuxinator"
                     ]
             $ rubyPackage "tmuxinator"
             )
    { snippets =
        [ Snippet zshrc $ unlines
              [ "export TMUXINATOR_CONFIG=\"$HOME/dotfiles/src/configs/tmuxinator\""
              , "source $HOME/dotfiles/.devfiles/.installations/tmuxinator/tmuxinator/completion/tmuxinator.zsh"
              ]
        ]
    }



zsh = Package
    { name         = "zsh"
    , source       = Batch [Apt "zsh", Custom Zsh.setShell]
    , dependencies = [ Package
                         { name         = "zprofile"
                         , source       = noopSource
                         , dependencies = []
                         , config       = Just $ PackageConfig ".zprofile" Home
                         , snippets     = []
                         }
                     , ( githubPackage "zsh-completions"
                       $ githubAddress "zsh-users" "zsh-completions"
                       )
                         { snippets =
                             [ Snippet zshrc $ unlines
                                   [ "export fpath=(path/to/zsh-completions/src $fpath)"
                                   , "rm -f ~/.zcompdump; compinit"
                                   ]
                             ]
                         }
                     ]
    , config       = Just zshrc
    , snippets     = []
    }

noopSource = Custom $ return ()

-- Configs

zshrc = PackageConfig ".zshrc" Home
vimrc = PackageConfig ".vimrc" Home

-- Registry

type Registry = HashMap.HashMap String Package

createRegistry :: [Package] -> Registry
createRegistry = HashMap.fromList
    . fmap (\package -> ((name :: Package -> String) package, package))

centralRegistry :: Registry
centralRegistry = createRegistry
    [ zsh
    , (githubPackage "oh-my-zsh" $ githubAddress "robbyrussell" "oh-my-zsh") { dependencies = [ zsh
                                                                                              ]
                                                                             }
    , zshTheme "powerlevel9k" $ githubAddress "bhilburn" "powerlevel9k"
    , aptPackage "fonts-powerline"
    ]

registryLookup registry packageName = HashMap.lookup packageName registry
registryMember registry packageName = HashMap.member packageName registry


installVimPlug =
    runProcess'
        "curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"

brewUpgrade name = runProcess ("brew upgrade " <> name)

updateBrewPackages = do
    outdatedPackages <- getOutdated <$> runProcess "brew outdated"
    logDebug $ "Outdated packages: " <> show outdatedPackages
    mapM_ brewUpgrade outdatedPackages
    where getOutdated = mapMaybe (headMay . words) . lines
