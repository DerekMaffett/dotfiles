module TerminalHappiness
    ( install
    )
where

import           Config
import           Process
import           Logger
import           Control.Monad.Reader

install = do
    installPowerlineFonts
    installTerminalColors


installPowerlineFonts = do
    Config { installationsDir } <- ask
    runProcess
        (  "git clone https://github.com/powerline/fonts.git --depth=1 "
        <> installationsDir
        <> "/powerline-fonts"
        )
        []
    runProcess (installationsDir <> "/powerline-fonts/install.sh") []


installTerminalColors = do
    Config { installationsDir } <- ask
    runProcess
        (  "git clone git@github.com:mbadolato/iTerm2-Color-Schemes.git "
        <> installationsDir
        <> "/iterm-colors"
        )
        []
