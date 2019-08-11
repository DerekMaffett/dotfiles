module Main where

import qualified Data.Aeson                    as A
import           GHC.Generics
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as TextIO
import qualified System.Directory              as Dir
import qualified System.Process                as P

data Config = Config
  { keys :: [Text]
  , defaultKey :: Text
  } deriving (Generic, A.FromJSON)


main :: IO ()
main = do
    homeDir <- Dir.getHomeDirectory
    let sshConfigPath     = homeDir <> "/.ssh/config"
        sshInitConfigPath = homeDir <> "/.ssh/ssh-init.json"

    maybeConfig <- A.decodeFileStrict sshInitConfigPath
    case maybeConfig of
        Nothing     -> fail $ sshInitConfigPath <> " not parseable"
        Just config -> do
            keygenFromConfig config
            TextIO.writeFile sshConfigPath $ convertToHostConfig config

keygenFromConfig (Config { keys, defaultKey }) = mapM_ (keygen defaultKey) keys

keygen defaultKey key = do
    homeDir <- Dir.getHomeDirectory
    P.callCommand
        .  T.unpack
        $  "ssh-keygen -t ecdsa -b 521 -f "
        <> T.pack homeDir
        <> "/.ssh/"
        <> (if key == defaultKey then "id_ecdsa" else key)

convertToHostConfig config =
    foldl (<>) ""
        $   convertKeyNameToHostConfigSnippet (defaultKey config)
        <$> keys config

convertKeyNameToHostConfigSnippet defaultKeyName keyName =
    T.unlines $ hostConfigFor "github.com" <> hostConfigFor "bitbucket.org"
  where
    hostConfigFor hostName =
        [ "Host " <> keyName <> "." <> hostName
        , "  HostName " <> hostName
        , "  IdentityFile ~/.ssh/"
            <> (if defaultKeyName == keyName then "id_ecdsa" else keyName)
        ]
