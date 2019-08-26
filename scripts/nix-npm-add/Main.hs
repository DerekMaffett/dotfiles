module Main where

import           System.Process                 ( callCommand )
import           System.Environment
import           Files                          ( alterJsonFile )

main :: IO ()
main = do
    pkgName <- head <$> getArgs
    alterJsonFile "dotfiles/configs/nodepkgs/node-packages.json" (pkgName :)
    callCommand
        "cd ~/dotfiles/configs/nodepkgs && node2nix --input node-packages.json"
