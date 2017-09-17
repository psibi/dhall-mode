;; -*- lexical-binding: t -*-

;; Create the syntax table for this mode.
(defvar dhall-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\  " " st)
    (modify-syntax-entry ?\t " " st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "_" st)
    (modify-syntax-entry ?_  "_" st)
    (modify-syntax-entry ?\[  "(]" st)
    (modify-syntax-entry ?\]  ")[" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?- ". 12" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table used while in `dhall-mode'.")

(define-derived-mode dhall-mode prog-mode "Dhall"
  "Major mode for editing Dhall files."
  :group 'dhall
  (setq font-lock-defaults (list nil))
  (set (make-local-variable 'comment-start) "--")
  (set-syntax-table dhall-mode-syntax-table)
  )
