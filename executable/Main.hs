module Main where

import Neovim
import qualified Neovim.Ghcid as Ghcid

main :: IO ()
main = neovim defaultConfig{plugins = [Ghcid.plugin]}
