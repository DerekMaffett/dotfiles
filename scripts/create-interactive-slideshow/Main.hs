module Main where

import           Tmuxinator
import           Files
import           System.Environment
import           System.Directory              as Dir
import           Data.Char
import           Data.List
import           Data.List.Split
import qualified Data.List.Index               as IL
import           Data.HashMap.Strict

main = do
    informalFilePath <- head <$> getArgs
    filePath         <- Dir.makeAbsolute informalFilePath
    let fileParentPath = dropWhileEnd (/= '/') filePath
    contents <- lines <$> readFile filePath
    let slides = splitWhen ("-- SLIDE" `isPrefixOf`) contents

    Dir.removePathForcibly $ fileParentPath <> "/.slides"
    Dir.createDirectoryIfMissing True $ fileParentPath <> "/.slides"

    IL.imapM_ (writeSlideFile fileParentPath) slides

    writeTmuxinatorConfig fileParentPath slides
  where
    writeSlideFile fileParentPath index slideContent = writeFile
        (fileParentPath <> "/.slides/" <> (show index) <> ".hs")
        (trim . unlines $ slideContent)
    trim = dropWhile isSpace . dropWhileEnd isSpace


writeTmuxinatorConfig fileParentPath slides = do
    writeYamlFile
        (fileParentPath <> ".slides/.slideshow.yml")
        TmuxinatorConfig
            { name    = "slideshow"
            , windows = IL.imap (slideToWindows fileParentPath) slides
            }

slideToWindows fileParentPath index slide =
    singleton (show index) $ WindowConfig
        { root   = fileParentPath
        , layout = "main-vertical"
        , panes  = [ Just $ "vim " <> slidePath
                   , Just $ "watch " <> show index
                   , Nothing
                   ]
        }
    where slidePath = fileParentPath <> ".slides/" <> show index <> ".hs"
