# nvim-hs-ghcid

Ghcid integration plugin for [`nvim-hs`](https://github.com/neovimhaskell/nvim-hs).

This plugin fills the quickfix list with location of compiler errors and warnings. Warnings are only added to the quickfix list if no errors are present.

# Installation

Add the plugin to your `nvim-hs` config file.

Sample configuration file:

```haskell

import Neovim

import qualified Neovim.Ghcid as Ghcid

main :: IO ()
main = neovim defaultConfig
    { plugins = defaultPlugins defaultConfig ++ [ Ghcid.plugin ]
    }
```
# Usage

> :GhcidStart

To initialize a Ghcid session which will fill the quickfix list on errors/warnings. If you add a bang, it will not ask you for the configuration and uses the last saved configuration for the project or guesses a configuration.

> :GhcidStop 

Stop the Ghcid session for the project in which the current file resides.

> :GhcidRestart

Same as `:GhcidStop` followed by `:GhcidStart!`.
