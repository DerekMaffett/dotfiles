module Main where

import           Files                          ( readJsonFile
                                                , alterJsonFile
                                                , writeJsonFile
                                                )

import           GHC.Generics
import           Options.Applicative
import           System.Process                 ( readProcess )
import qualified Data.Aeson                    as A
import           Data.Functor
import           Data.Maybe
import qualified Data.HashMap.Strict           as HM
import           Data.List.Split
import           Data.ByteString.Lazy.Char8     ( pack )
import qualified Control.Monad.Parallel        as P

data Command
  = Add String
  | Update

addOpts :: ParserInfo Command
addOpts = Add <$> info
    (strOption $ long "repo" <> short 'r' <> metavar "REPO_ADDRESS" <> help
        "repo address"
    )
    (fullDesc <> progDesc "adds repo in format org/repo" <> header "adds repo")

updateOpts :: ParserInfo Command
updateOpts = Update <$ info
    (pure ())
    (fullDesc <> progDesc "Updates existing repo pointers" <> header
        "Updates existing repo pointers"
    )

opts :: ParserInfo Command
opts = info optsParser desc
  where
    optsParser =
        subparser (command "add" addOpts <> command "update" updateOpts)
            <**> helper
    desc = fullDesc <> progDesc "Handles repo pointers for nix"

main = do
    command <- execParser opts
    case command of
        Add repo -> addRepo repo
        Update   -> updateRepos


data PrefetchData = PrefetchData
    { owner :: String
    , repo :: String
    , rev :: String
    , sha256 :: String
   } deriving (Generic, A.FromJSON, A.ToJSON)

centralPkgList = "dotfiles/configs/github-pkgs/github-pkgs.json"
compiledPkgList = "dotfiles/configs/github-pkgs/compiled-github-pkgs.json"

addRepo repoAddress = do
    alterJsonFile centralPkgList (repoAddress :)
    updateRepos

updateRepos = do
    repos :: [String] <- readJsonFile centralPkgList
    newContents       <- foldToHashMap <$> P.mapM prefetchGithub repos
    writeJsonFile compiledPkgList newContents

prefetchGithub :: String -> IO PrefetchData
prefetchGithub repoAddress =
    let org : repo : _ = splitOn "/" repoAddress
    in  do
            prefetchResult <- readProcess "nix-prefetch-github" [org, repo] []
            putStrLn $ "Updated " <> org <> "/" <> repo
            return $ fromJust <$> A.decode $ pack prefetchResult

foldToHashMap prefetchResults =
    foldl (<>) HM.empty . map createSingleton $ prefetchResults
  where
    createSingleton prefetchResult =
        HM.singleton (repo prefetchResult) prefetchResult
