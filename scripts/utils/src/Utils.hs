module Utils
    ( alterYamlFile
    , alterJsonFile
    , callCommand
    , FromJSON
    , ToJSON
    )
where

import           System.Directory               ( getHomeDirectory )
import           System.Process                 ( callCommand )
import           Data.Yaml                      ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.Yaml                     as Y
import qualified Data.Aeson                    as A

alterYamlFile filePath alterFn = do
    homeDir      <- getHomeDirectory
    fileContents <- Y.decodeFileThrow $ homeDir <> "/" <> filePath
    Y.encodeFile (homeDir <> "/" <> filePath) (alterFn fileContents)

alterJsonFile filePath alterFn = do
    homeDir            <- getHomeDirectory
    eitherFileContents <- A.eitherDecodeFileStrict $ homeDir <> "/" <> filePath
    case eitherFileContents of
        Left  err          -> fail err
        Right fileContents -> do
            A.encodeFile (homeDir <> "/" <> filePath) (alterFn fileContents)
