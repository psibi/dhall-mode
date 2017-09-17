;; -*- lexical-binding: t -*-

;; Author:  2017 Sibi Prabakaran
;; Keywords: Dhall mode
;; Version: 0.1
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

;; Create the syntax table for this mode.
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
(setq dhall-mode-keywords '("if" "then" "else" "let" "in" "using") )
(setq dhall-mode-types '("Optional" "Bool" "Natural" "Integer" "Double" "Text" "List" "Type"))
(setq dhall-mode-constants '("True" "False"))

(setq dhall-mode-keywords-regexp (regexp-opt dhall-mode-keywords 'words))
(setq dhall-mode-type-regexp (regexp-opt dhall-mode-types 'words))
(setq dhall-mode-constant-regexp (regexp-opt dhall-mode-constants 'words))

;; Todo: Move away to proper multi line font lock methods
(defconst dhall-mode-multiline-string-regexp
  "''[^']*''"
  "Regular expression for matching multiline dhall strings.")


(defconst dhall-mode-font-lock-keywords
  `(;; Variables
    (,dhall-mode-type-regexp . font-lock-type-face)
    (,dhall-mode-constant-regexp . font-lock-constant-face)
    (,dhall-mode-keywords-regexp . font-lock-keyword-face)
    (,dhall-mode-multiline-string-regexp . font-lock-string-face)
    )
  )

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

(provide 'dhall-mode)
