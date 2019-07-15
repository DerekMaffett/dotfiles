module Main where

import           System.Process
import           Data.List
import           Data.Char

main :: IO ()
main = do
    currentBranch <-
        trim <$> readCreateProcess (shell "git rev-parse --abbrev-ref HEAD") []
    putStr currentBranch
    where trim = dropWhile isSpace . dropWhileEnd isSpace
