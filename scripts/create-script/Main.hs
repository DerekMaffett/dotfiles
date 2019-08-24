module Main where

import           GHC.Generics
import           System.Directory
import           System.Environment
import           Utils                          ( callCommand
                                                , alterYamlFile
                                                , FromJSON
                                                , ToJSON
                                                )
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

main :: IO ()
main = do
    scriptName <- head <$> getArgs
    createScript scriptName
    alterYamlFile "dotfiles/configs/tmuxinator/dotfiles.yml"
        $ updateContents scriptName

createScript scriptName =
    callCommand
        $  "cd ~/dotfiles/scripts/ && stack new "
        <> scriptName
        <> " script.hsfiles"

updateContents scriptName dotfilesConfig = dotfilesConfig
    { windows = (windows dotfilesConfig)
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
