{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  Neovim.Ghcid
Description :  Ghcid plugin
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC
-}
module Neovim.Ghcid where

import Neovim
import Neovim.API.String

import Neovim.Ghcid.Plugin

plugin :: Neovim () NeovimPlugin
plugin = do
    _ <- vim_command "sign define GhcidWarn text=>> texthl=Search"
    _ <- vim_command "sign define GhcidErr text=!! texthl=ErrorMsg"
    env <- initGhcidEnv
    wrapPlugin
        Plugin
            { environment = env
            , exports =
                [ $(command' 'ghcidStart) ["async", "!"]
                , $(command' 'ghcidStop) ["async"]
                , $(command' 'ghcidRestart) ["async"]
                ]
            }
