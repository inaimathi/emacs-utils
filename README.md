My Emacs Utils
==============

These are my personal utilities. They're made to be relatively easy to use, but ease of installation was not considered (they were created to solve minor annoyances I had with my workflow). Use them if you can, fork them if you like; this repo is released under the GPL3.

# Tagariffic

### Summary

Shortcuts for visiting and creating etags tables.

### Usage 

Put the `tagariffic.el` file somewhere in your load-path, add `(require 'tagariffic)` and 

    (global-set-key (kbd "C-.") 'visit-tags-table)
    (global-set-key (kbd "C->") 'create-tag-table)
    
to your `.emacs` file. 

Tagging Haskell files assumes you have [hasktags](http://hackage.haskell.org/package/hasktags) installed.

# blog-mode

### Summary

Editing shortcuts for blogging. Highlights are the link, code and footnote generators.

### Usage

Put the `blog-mode.el` file somewhere in your load-path and add `(require 'blog-mode)` to your `.emacs`. I also like to add `(add-hook 'html-mode-hook 'blog-mode)`, and `byte-compile-file` it.

Code blocks assume you have [htmlize](http://www.emacswiki.org/emacs/Htmlize) installed (`blog-mode` requires it though, so you just need to have the file in your load-path).
