My Emacs Utils
==============

These are my personal utilities. They're made to be relatively easy to use, but ease of installation was not considered (they were created to solve minor annoyances I had with my workflow). Use them if you can, fork them if you like; this repo is released under the GPL3.

### Usage 

Put the folder somewhere in your load path, `byte-compile-file` them, and require the desired modules in your `.emacs`. I also add `(add-hook 'html-mode-hook 'blog-mode)` (since I always want `blog-mode` on when I'm editing HTML) and add `tagariffic` to any mode-hooks that might use it. The require statements are

 - `(require 'tagariffic)`
 - `(require 'blog-mode)`
 - `(require 'git-custom)`
 - `(require 'erl-custom)`
 - `(require 'teensy-mode)`
 - `(require 'convenience)` (this is included with blog-mode and teensy-mode)

### Dependencies

`erl-custom` uses `shell-command` to call out to `git`.

`tagariffic` works without dependencies, but can't tag Haskell projects unless you have [hasktags](http://hackage.haskell.org/package/hasktags) installed.

`blog-mode` works without dependencies, but can't format code for you unless you have [htmlize](http://www.emacswiki.org/emacs/Htmlize) installed and included. It also uses `x-get-clipboard` to format link addresses (so you'll need to tweak it if you're a windows or OS X user). It also assumes you have the `convenience` package from this repo in your load path.

`convenience` assumes you have the `cl` package. It isn't actually a mode, but rather convenience functions for writing modes. It includes an elisp-macroexpander, word/character counters and shorthand macros for keymap/global mode definitions

`teensy-mode` assumes you have the `convenience` package. The default options also assume you have `gnu-make`, `gcc` and `teensy_loader_cli` installed (though you can change what all of them use through `M-x customize-group teensy-mode`)

# tagariffic

#### Shortcuts for visiting and creating etags tables.

Adds a minor mode with two keybindings, `C-.` and `C->`. They allow you to manage your tags files more easily for given projects. As it stands, they only tag one language type at a time (I don't typically work in projects with multiple heavily-used languages, so I only need to tag the primary one).

# blog-mode

#### Editing shortcuts for blogging. Highlights are the link, code and footnote generators.

Minor mode that adds a number of keyboard shortcuts for inserting/regioning various tags. It also manages linked footnotes automatically.

# erl-custom

#### Utility functions to generate basic Erlang project skeletons

I got sick of writing the same `.app`, `_app.erl`, `_supervisor.erl` and `Makefile` skeletons every time I start a new Erlang project. So now I don't have to. Note that this is a "mode" for purposes of `require`, but doesn't actually create a new mode and doesn't assign keybindings to any of the functions it defines. The only one I really use is `erl-custom-template-project`, and that gets run so seldom that I didn't want to clutter my keyboard shortcuts out of the box. YMMV, of course.

The functions mostly follow the OTP design principles; they automatically organize things into `src`, `deps` and `ebin` directories, but don't use a `rel` directory, don't separate non-erlang code into separate directories, and use `screen` rather than built-in Erlang settings to daemonize things. I should probably work on that. 

# teensy-mode

#### Extra keystrokes for Teensy development in C

It's just basic scripts that enable compiling, project-compiling and loading onto a Teensy board through Emacs. 
