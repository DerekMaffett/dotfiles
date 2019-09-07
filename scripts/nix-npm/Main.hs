module Main where

import           Options.Applicative
import           System.Process                 ( callCommand )
import           System.Environment
import           Files                          ( alterJsonFile )


data Command
  = Add String
  | Update

main = do
    command <- execParser opts
    case command of
        Add packageName -> addPackage packageName
        Update          -> updatePackages

addOpts :: ParserInfo Command
addOpts = Add <$> info
    (strOption $ long "package" <> short 'p' <> metavar "PACKAGE_NAME" <> help
        "package name"
    )
    (fullDesc <> progDesc "adds package" <> header "adds package")

updateOpts :: ParserInfo Command
updateOpts = Update <$ info
    (pure ())
    (fullDesc <> progDesc "Updates existing node packages" <> header
        "Updates existing node packages"
    )

opts :: ParserInfo Command
opts = info optsParser desc
  where
    optsParser =
        subparser (command "add" addOpts <> command "update" updateOpts)
            <**> helper
    desc = fullDesc <> progDesc "Handles node packages for nix"

addPackage pkgName = do
    alterJsonFile "dotfiles/configs/nodepkgs/node-packages.json" (pkgName :)

updatePackages = do
    callCommand
        "cd ~/dotfiles/configs/nodepkgs && node2nix --input node-packages.json"
