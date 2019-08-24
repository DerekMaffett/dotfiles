module Main where

import           System.Environment
import           Utils                          ( callCommand
                                                , alterJsonFile
                                                )

main :: IO ()
main = do
    pkgName <- head <$> getArgs
    alterJsonFile "dotfiles/configs/nodepkgs/node-packages.json" (pkgName :)
    callCommand
        "cd ~/dotfiles/configs/nodepkgs && node2nix --input node-packages.json"
