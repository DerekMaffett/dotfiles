module Packages
    ( install
    , processPackageList
    )
where

import           Logger
import           Data.Maybe
import           Debug.Trace
import           Data.List
import           Control.Monad
import qualified Installer
import           Registry                       ( registryLookup
                                                , registryMember
                                                , Package(name, dependencies)
                                                , centralRegistry
                                                , Registry
                                                )

stringSources =
    [ "brittany"
    , "prettier"
    , "oh-my-zsh"
    , "neovim"
    , "elm"
    , "elm-format"
    , "elm-test"
    , "autojump"
    , "cloc"
    , "tmux"
    , "the_silver_searcher"
    , "vim-plug"
    , "tmuxinator"
    , "zsh"
    , "powerlevel9k"
    , "powerline-fonts"
    , "iTerm2-color-schemes"
    ]


unpackSources registry sourceList = registryLookup registry <$> sourceList

expandDependenciesList packages =
    (nubBy (\x y -> name x == name y))
        . (concatMap expandDependencies)
        $ packages
  where
    expandDependencies package = case (dependencies package) of
        [] -> [package]
        xs -> expandDependenciesList xs <> [package]

isMissing registry = not . registryMember registry

processPackageList :: Registry -> [String] -> [Package]
processPackageList registry packageList =
    expandDependenciesList
        . catMaybes
        . (unpackSources registry)
        . nub
        $ packageList

install = do
    when (not . null $ missingPackages)
        $ logError ("MISSING REGISTRY PACKAGES: " <> show missingPackages)
    (mapM_ Installer.installPackage)
        $ traceShow (name <$> packagesToInstall) packagesToInstall
  where
    packagesToInstall = processPackageList centralRegistry stringSources
    missingPackages =
        (filter $ isMissing centralRegistry) . nub $ stringSources
