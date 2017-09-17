;; -*- lexical-binding: t -*-

(require 'highlight-numbers-mode)
(require 'rainbow-delimiters)

;; Create the syntax table for this mode.
(defvar dhall-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\  " " st)
    (modify-syntax-entry ?\t " " st)
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
(setq dhall-mode-keywords '("if" "then" "else" "let" "in") )
(setq dhall-mode-types '("Optional" "Bool" "Integer" "Double" "Text" "List" "Type"))
(setq dhall-mode-constants '("True" "False"))

(setq dhall-mode-keywords-regexp (regexp-opt dhall-mode-keywords 'words))
(setq dhall-mode-type-regexp (regexp-opt dhall-mode-types 'words))
(setq dhall-mode-constant-regexp (regexp-opt dhall-mode-constants 'words))

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
  (set-syntax-table dhall-mode-syntax-table)
  )

(provide 'dhall-mode)
