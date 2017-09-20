;;; dhall-mode.el --- a major mode for dhall configuration language -*- lexical-binding: t -*-

;; Copyright (C) 2017 Sibi Prabakaran

;; Author: Sibi Prabakaran <sibi@psibi.in>
;; Maintainer: Sibi Prabakaran <sibi@psibi.in>
;; Keywords: languages
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (highlight-numbers "0.2.3") (rainbow-delimiters "2.1.3") (highlight-operators "0.1"))
;; URL: https://github.com/psibi/dhall-mode

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for editing Dhall configuration file (See
;; https://github.com/dhall-lang/dhall-lang to learn more) in Emacs.
;;
;; Some of its major features include:
;;
;;  - syntax highlighting (font lock),
;;
;;  - Basic indendation
;;
;;  - Error highlighting on unbalanced record, parenthesis in functions
;;
;; Todo: Add REPL support and automatic formatting on save
;;

(require 'highlight-numbers)
(require 'rainbow-delimiters)
(require 'highlight-operators)

(defconst dhall-mode-version "0.1.0"
  "Dhall Mode version.")


(defgroup dhall-mode nil
  "Major mode for editing dhall files"
  :group 'languages
  :prefix "dhall-"
  :link '(url-link :tag "Site" "https://github.com/psibi/dhall-mode")
  :link '(url-link :tag "Repository" "https://github.com/psibi/dhall-mode"))

;; Create the syntax table for this mode.
;;; Code:

(defvar dhall-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\  " " st)
    (modify-syntax-entry ?\t " " st)
    (modify-syntax-entry ?\\ "_" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\[  "(]" st)
    (modify-syntax-entry ?\]  ")[" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?- ". 12" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table used while in `dhall-mode'.")

;; define several category of keywords
(defvar dhall-mode-keywords
  (regexp-opt '("if" "then" "else" "let" "in" "using")))

(defvar dhall-mode-types
  (regexp-opt
  '("Optional" "Bool" "Natural" "Integer" "Double" "Text" "List" "Type")))

(defvar dhall-mode-types (regexp-opt
'("Optional" "Bool" "Natural" "Integer" "Double" "Text" "List" "Type")))

(defvar dhall-mode-constants (regexp-opt '("True" "False")))

;; Todo: Move away to proper multi line font lock methods
(defconst dhall-mode-multiline-string-regexp
  "''[^']*''"
  "Regular expression for matching multiline dhall strings.")

(defconst dhall-mode-font-lock-keywords
  `(;; Variables
    (,dhall-mode-types . font-lock-type-face)
    (,dhall-mode-constants . font-lock-constant-face)
    (,dhall-mode-keywords . font-lock-keyword-face)
    (,dhall-mode-multiline-string-regexp . font-lock-string-face)
    )
  )

;; The main mode functions
;;;###autoload
(define-derived-mode dhall-mode prog-mode "Dhall"
  "Major mode for editing Dhall files."
  :group 'dhall
  (setq font-lock-defaults '((dhall-mode-font-lock-keywords) nil nil))
  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'font-lock-multiline) t)
  (setq-local indent-tabs-mode t)
  (setq-local tab-width 4)
  (highlight-numbers-mode 1)
  (rainbow-delimiters-mode 1)
  (highlight-operators-mode 1)
  (set-syntax-table dhall-mode-syntax-table)
  )

;; Provide ourselves:
(provide 'dhall-mode)
;;; dhall-mode.el ends here
