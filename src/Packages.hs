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
                                                , Package(..)
                                                , centralRegistry
                                                , Registry
                                                )

stringSources =
    [ "git"
    , "brittany"
    , "prettier"
    , "hidden-dock"
    , "tmuxinator"
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

getConfigsAndSnippets packages = catMaybes . (fmap config) $ packages

install = do
    when (not . null $ missingPackages)
        $ logError ("MISSING REGISTRY PACKAGES: " <> show missingPackages)
    (mapM_ Installer.installConfig) . getConfigsAndSnippets $ packagesToInstall
    mapM_ Installer.installPackage packagesToInstall
  where
    packagesToInstall = processPackageList centralRegistry stringSources
    missingPackages =
        (filter $ isMissing centralRegistry) . nub $ stringSources
