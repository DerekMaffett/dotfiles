module Registry.Vim
    ( make
    )
where

import           Process
import           Control.Monad.Reader
import           Config
import           Symlinks

make = do
    Config { buildDir, installationsDir, binDir } <- ask
    runProcessWithDir
        (getNeovimDir installationsDir)
        (  "make CMAKE_EXTRA_FLAGS=\"-DCMAKE_INSTALL_PREFIX="
        <> buildDir
        <> "/neovim\""
        )
    runProcessWithDir (getNeovimDir installationsDir) "make install"
    createSymlink (buildDir <> "/neovim/bin/nvim") (binDir <> "/nvim")
  where
    getNeovimDir installationsDir = (installationsDir <> "/neovim/neovim")
