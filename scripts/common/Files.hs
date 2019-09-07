module Files where

import           System.Directory               ( getHomeDirectory )
import qualified Data.Yaml                     as Y
import qualified Data.Aeson                    as A

alterYamlFile filePath alterFn = do
    homeDir      <- getHomeDirectory
    fileContents <- Y.decodeFileThrow $ homeDir <> "/" <> filePath
    Y.encodeFile (homeDir <> "/" <> filePath) (alterFn fileContents)

writeYamlFile :: (Y.ToJSON a) => String -> a -> IO ()
writeYamlFile filePath content = do
    Y.encodeFile filePath content

writeJsonFile :: (A.FromJSON a, A.ToJSON a) => String -> a -> IO ()
writeJsonFile filePath contents = do
    homeDir <- getHomeDirectory
    A.encodeFile (homeDir <> "/" <> filePath) contents


alterJsonFile :: (A.FromJSON a, A.ToJSON a) => String -> (a -> a) -> IO ()
alterJsonFile filePath alterFn = do
    homeDir      <- getHomeDirectory
    fileContents <- readJsonFile filePath
    A.encodeFile (homeDir <> "/" <> filePath) (alterFn fileContents)

readJsonFile :: (A.FromJSON a) => String -> IO a
readJsonFile filePath = do
    homeDir            <- getHomeDirectory
    eitherFileContents <- A.eitherDecodeFileStrict $ homeDir <> "/" <> filePath
    case eitherFileContents of
        Left  err          -> fail err
        Right fileContents -> return fileContents
