module Main where

import           GHC.Generics
import           System.Directory
import           System.Environment
import           System.Process                 ( callCommand )
import           Data.Yaml
import           Data.HashMap.Strict

data TmuxinatorConfig = TmuxinatorConfig
    { name :: String
    , windows :: [HashMap String WindowConfig]
    } deriving (Generic, FromJSON, ToJSON)

data WindowConfig = WindowConfig
    { root :: String
    , layout :: String
    , panes :: [Maybe String]
    } deriving (Generic, FromJSON, ToJSON)

-- instance FromJSON WindowConfig where
--     parseJSON = withObject "object" $ \o -> do
--         username <- o .: "username"
--         password <- o .: "password"
--         return $ BasicAuthCredentials username password



main :: IO ()
main = do
    scriptName <- head <$> getArgs
    createScript scriptName
    dotfilesTmuxinatorConfig <- getDotfilesConfig
    writeDotfilesConfig scriptName dotfilesTmuxinatorConfig

createScript scriptName =
    callCommand
        $  "cd ~/dotfiles/scripts/ && stack new "
        <> scriptName
        <> " script.hsfiles"

getDotfilesConfig = do
    homeDir <- getHomeDirectory
    decodeFileThrow $ homeDir <> "/dotfiles/configs/tmuxinator/dotfiles.yml"

writeDotfilesConfig scriptName dotfilesConfig =
    let newConfig = dotfilesConfig
            { windows =
                (windows dotfilesConfig)
                    <> [ singleton scriptName $ WindowConfig
                             { root   = "~/dotfiles/scripts/" <> scriptName
                             , layout = "main-vertical"
                             , panes  = [ Just "vim"
                                        , Just "stack install --file-watch"
                                        , Nothing
                                        ]
                             }
                       ]
            }
    in  do

            homeDir <- getHomeDirectory
            encodeFile
                (homeDir <> "/dotfiles/configs/tmuxinator/dotfiles.yml")
                newConfig
