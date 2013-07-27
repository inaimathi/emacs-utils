My Emacs Utils
==============

These are my personal utilities. They're made to be relatively easy to use, but ease of installation was not considered (they were created to solve minor annoyances I had with my workflow). Use them if you can, fork them if you like; this repo is released under the GPL3.

### Usage 

Put the folder somewhere in your load path, `byte-compile-file` them, and require the desired modules in your `.emacs`. I also add `(add-hook 'html-mode-hook 'blog-mode)` (since I always want `blog-mode` on when I'm editing HTML). The require statements are

 - `(require 'blog-mode)`
 - `(require 'convenience)` (this is included with blog-mode and teensy-mode)
 - `(require 'ha-custom)`

### Dependencies

`ha-custom` depends on a local installation of `hoogle` (which you can get using `cabal`) and `hlint` (which is available through `apt-get`). If you want to use other programs for similar aims, the commands used to run these are customizable.

`blog-mode` works without dependencies, but can't format code for you unless you have [htmlize](http://www.emacswiki.org/emacs/Htmlize) installed and included. It also uses `x-get-clipboard` to format link addresses (so you'll need to tweak it if you're a windows or OS X user). It also assumes you have the `convenience` package from this repo in your load path.

`convenience` assumes you have the `cl` package. It isn't actually a mode, but rather convenience functions for writing modes. It includes an elisp-macroexpander, word/character counters and shorthand macros for keymap/global mode definitions

`teensy-mode` assumes you have the `convenience` package. The default options also assume you have `gnu-make`, `gcc` and `teensy_loader_cli` installed (though you can change what all of them use through `M-x customize-group teensy-mode`)

# blog-mode

#### Editing shortcuts for blogging. Highlights are the link, code and footnote generators.

Minor mode that adds a number of keyboard shortcuts for inserting/regioning various tags. It also manages linked footnotes automatically.

# ha-custom

#### Utility functions for documentation and linting in haskell-mode

Really only three relevant interactive commands here. `ha-custom-hoogle-search` searches a local hoogle installation for the given search term, `ha-custom-hoogle-doc` shows the doc page for the first result of a given search term, `ha-custom-lint` runs the haskell linter on the current buffer. Once a file has been run through the linter, `ha-custom-do-next-replacement` applies a replacement.

# teensy-mode

#### Extra keystrokes for Teensy development in C

It's just basic scripts that enable compiling, project-compiling and loading onto a Teensy board through Emacs. 
