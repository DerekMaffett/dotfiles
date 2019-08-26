module Tmuxinator where

import           GHC.Generics
import qualified Data.Yaml                     as Y
import           Data.HashMap.Strict


data TmuxinatorConfig = TmuxinatorConfig
    { name :: String
    , windows :: [HashMap String WindowConfig]
    } deriving (Generic, Y.FromJSON, Y.ToJSON)

data WindowConfig = WindowConfig
    { root :: String
    , layout :: String
    , panes :: [Maybe String]
    } deriving (Generic, Y.FromJSON, Y.ToJSON)

addToTmuxinatorWorkspace name windowConfig oldConfig =
    oldConfig { windows = (windows oldConfig) <> [singleton name windowConfig] }
