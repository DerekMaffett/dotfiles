module Symlinks
  ( createSymlinks
  )
where

import           System.Directory              as Dir
import           Data.Semigroup                 ( (<>) )
import           Control.Monad

configs =
  [ "gitconfig"
  , "bashrc"
  , "bash_profile"
  , "bash"
  , "vimrc"
  , "tmux.conf"
  , "agignore"
  ]

append = flip (<>)

createSymlink targetPath linkPath = do
  exists <- Dir.doesPathExist linkPath
  when exists $ Dir.removeFile linkPath
  Dir.createFileLink targetPath linkPath

linkToHomeDir configsDir homeDir configName = createSymlink
  (configsDir <> "/" <> configName)
  (homeDir <> "/." <> configName)

createSymlinks = do
  homeDir    <- Dir.getHomeDirectory
  configsDir <- append "/src/configs" <$> Dir.getCurrentDirectory
  mapM_ (linkToHomeDir configsDir homeDir) configs
  Dir.createDirectoryIfMissing False (homeDir <> "/.config/nvim")
  createSymlink (configsDir <> "/" <> "init.vim")
                (homeDir <> "/.config/nvim/init.vim")
