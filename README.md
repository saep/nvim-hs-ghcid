# nvim-hs-ghcid

Ghcid integration plugin for [`nvim-hs`](https://github.com/neovimhaskell/nvim-hs).

This plugin fills the quickfix list with location of compiler errors and warnings. Warnings are only added to the quickfix list if no errors are present.

# Disclaimer

I don't know if anyone uses this plugin, but I (saep) have not been using it for years. I usually compile it and test it shortly if it stops compiling on stackage, but that's about it. It can still be a useful template to write your own plugin as it is as complicated as it gets for a plugin.

If you want to have a development environment to program haskell in, try [haskell-language-server](https://github.com/haskell/haskell-language-server) which handles different compiler versions and has far more features.

# Installation

You need [stack](https://github.com/commercialhaskell/stack) for the automatic compilation and starting of the plugin to work.

Then add the plugin and its dependency to your `neovim` config file (Example uses [vim-plug](https://github.com/junegunn/vim-plug)):

```viml
Plug 'neovimhaskell/nvim-hs.vim'
Plug 'saep/nvim-hs-ghcid'
```
# Usage

> :GhcidStart

To initialize a Ghcid session which will fill the quickfix list on errors/warnings. If you add a bang, it will not ask you for the configuration and uses the last saved configuration for the project or guesses a configuration.

> :GhcidStop 

Stop the Ghcid session for the project in which the current file resides.

> :GhcidRestart

Same as `:GhcidStop` followed by `:GhcidStart!`.
