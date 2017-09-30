# dhall-mode

[![MELPA](https://melpa.org/packages/dhall-mode-badge.svg)](https://melpa.org/#/dhall-mode)

Emacs Major mode for working
with [Dhall](https://github.com/dhall-lang/dhall-lang) configuration
language.

## Installation

* Make sure that you
  install [dhall-format](https://github.com/dhall-lang/dhall-haskell)
  and it's PATH is available to emacs via `exec-path`.
* Install this extension from MELPA:

``` emacs-lisp
(use-package dhall-mode
  :ensure t
  :mode "\\.dhall\\'")
```

## Demo

![Dhall in Emacs](https://user-images.githubusercontent.com/737477/31044377-e2af0e9e-a5eb-11e7-9757-806ae1448c40.gif "Dhall mode in Emacs")

## Features

* Syntax highlighting (Using font lock)
* Basic indendation
* Automatic formmating on save (Configurable via variable)
* Error highlighting.

Todo:

* Add REPL support. See: https://github.com/dhall-lang/dhall-lang/issues/17

## License

Copyright © 2017 Sibi Prabakaran

Distributed under GNU GPL, version 3.
