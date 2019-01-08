module Zsh
    ( install
    )
where

import           Process
import           Logger
import           Config
import           Control.Monad
import           Control.Monad.Reader
import           Data.List

data PluginType = Theme | Plugin

data ZshPlugin =
  ZshPlugin { author :: String
            , name :: String
            , pluginType :: PluginType
            }

zshPlugins =
    [ ZshPlugin {author = "bhilburn", name = "powerlevel9k", pluginType = Theme}
    , ZshPlugin
        { author     = "zsh-users"
        , name       = "zsh-completions"
        , pluginType = Plugin
        }
    ]

install = do
    installZsh
    installOhMyZsh
    installOhMyZshPlugins

installZsh = do
    output <- dropWhileEnd (== '\n') <$> runProcess "echo $SHELL"
    logNotice "Setting up zsh..."
    logDebug $ "Current shell: \"" <> output <> "\""
    when (output /= "/bin/zsh") (runProcess' "chsh -s $(which zsh)")


installOhMyZsh = do
    logNotice "Installing Oh My Zsh..."
    runProcess "rm -rf ~/.oh-my-zsh"
    runProcess
        "git clone https://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh"


installOhMyZshPlugins = mapM_ zshInstall zshPlugins


zshInstall package = do
    runProcess
        (  "git clone https://github.com/"
        <> author package
        <> "/"
        <> name package
        <> ".git "
        <> packagePath
        )
  where
    packagePath =
        "~/.oh-my-zsh/custom/" <> installLocation <> "/" <> name package
    installLocation = case pluginType package of
        Theme  -> "themes"
        Plugin -> "plugins"
