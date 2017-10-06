;;; dhall-mode.el --- a major mode for dhall configuration language -*- lexical-binding: t -*-

;; Copyright (C) 2017 Sibi Prabakaran

;; Author: Sibi Prabakaran <sibi@psibi.in>
;; Maintainer: Sibi Prabakaran <sibi@psibi.in>
;; Keywords: languages
;; Version: 0.1.1
;; Package-Requires: ((emacs "24.4") (ansi-color "3.0"))
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
;;  - Automatic formatting on save (configuratle via variable)
;;
;;  - Error highlighting
;;
;;; Code:

(require 'ansi-color)

(defconst dhall-mode-version "0.1.1" 
  "Dhall Mode version.")

(defgroup dhall nil 
  "Major mode for editing dhall files" 
  :group 'languages 
  :prefix "dhall-" 
  :link '(url-link :tag "Site" "https://github.com/psibi/dhall-mode") 
  :link '(url-link :tag "Repository" "https://github.com/psibi/dhall-mode"))

;; Create the syntax table for this mode.
(defvar dhall-mode-syntax-table 
  (let ((st (make-syntax-table))) 
        ;; Taken from haskell-mode: https://stackoverflow.com/a/20845468/1651941
    (modify-syntax-entry ?\{  "(}1nb" st)
    (modify-syntax-entry ?\}  "){4nb" st)
    (modify-syntax-entry ?-  "_ 123" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\  " " st) 
    (modify-syntax-entry ?\t " " st)

    ;; End...
    (modify-syntax-entry ?\[  "(]" st) 
    (modify-syntax-entry ?\]  ")[" st) 
    (modify-syntax-entry ?\( "()" st) 
    (modify-syntax-entry ?\) ")(" st) 
    ;; Let us handle escapes and string
    (modify-syntax-entry ?\\ "." st)
    (modify-syntax-entry ?\" "." st)
    ;; End

    st)
  "Syntax table used while in `dhall-mode'.")

;; define several category of keywords
(defvar dhall-mode-keywords (regexp-opt '("if" "then" "else" "let" "in" "using")))

(defvar dhall-mode-types 
  (regexp-opt '("Optional" "Bool" "Natural" "Integer" "Double" "Text" "List" "Type")))

(defvar dhall-mode-types 
  (regexp-opt '("Optional" "Bool" "Natural" "Integer" "Double" "Text" "List" "Type")))

(defvar dhall-mode-constants (regexp-opt '("True" "False")))
(defvar dhall-mode-numerals "+[1-9]")
(defvar dhall-mode-doubles "[0-9]\.[0-9]+")
(defvar dhall-mode-operators "->\\|\\[\\|]\\|,\\|:\\|=\\|\\\\\(\\|)\\|&&\\|||\\|{\\|}\\|(")
(defvar dhall-mode-variables "\\([a-zA-Z]+\\) *\t*=")

(defconst dhall-mode-font-lock-keywords 
  `( ;; Variables
    (,dhall-mode-types . font-lock-type-face) 
    (,dhall-mode-constants . font-lock-constant-face) 
    (,dhall-mode-operators . font-lock-builtin-face) 
    (,dhall-mode-variables . (1 font-lock-variable-name-face)) 
    (,dhall-mode-keywords . font-lock-keyword-face) 
    (,dhall-mode-doubles . font-lock-constant-face) 
    (,dhall-mode-numerals . font-lock-constant-face) 
    ))

(defcustom dhall-format-command "dhall-format" 
  "Command used to format Dhall files.
Should be dhall or the complete path to your dhall executable,
  e.g.: /home/sibi/.local/bin/dhall-format" 
  :type 'file 
  :group 'dhall 
  :safe 'stringp)

(defcustom dhall-format-at-save t 
  "If non-nil, the Dhal buffers will be formatted after each save." 
  :type 'boolean 
  :group 'dhall 
  :safe 'booleanp)

(defcustom dhall-format-options '("--inplace") 
  "Command line options for dhall-format executable." 
  :type '(repeat string) 
  :group 'dhall 
  :safe t)

(defun dhall-format () 
  "Formats the current buffer using dhall-format." 
  (interactive) 
  (message "Formatting Dhall file") 
  (let* ((ext (file-name-extension buffer-file-name t)) 
         (bufferfile (make-temp-file "dhall" nil ext)) 
         (curbuf (current-buffer)) 
         (errbuf (get-buffer-create "*dhall errors*")) 
         (coding-system-for-read 'utf-8) 
         (coding-system-for-write 'utf-8)) 
    (unwind-protect 
        (save-restriction 
          (widen) 
          (write-region nil nil bufferfile) 
          (with-current-buffer errbuf 
            (erase-buffer)) 
          (apply 'call-process dhall-format-command nil errbuf t (append dhall-format-options (list
                                                                                               (buffer-file-name)))) 
          (with-current-buffer errbuf 
            (save-restriction 
              (widen) 
              (let* ((errContent 
                      (buffer-substring-no-properties 
                       (point-min) 
                       (point-max))) 
                     (errLength (length errContent))) 
                (if (eq errLength 0) 
                    (progn
                      ;; (delete-window (get-buffer-window errbuf))
                      (with-current-buffer curbuf 
                        (revert-buffer 
                         :ignore-auto 

                         :noconfirm 
                         :preserve-modes))) 
                  (progn 
                    (ansi-color-apply-on-region (point-min) 
                                                (point-max)) 
                    (display-buffer errbuf))))))) 
      (delete-file bufferfile))))

(defun dhall-format-maybe () 
  "Run `dhall-format' if `dhall-format-at-save' is non-nil." 
  (if dhall-format-at-save
      (dhall-format)))

(defun dhall--get-parse-state (pos)
  "Get the result of `syntax-ppss' at POS."
  (save-excursion (save-match-data (syntax-ppss pos))))

(defun dhall--get-string-type (parse-state)
  "Get the type of string based on PARSE-STATE."
  (let ((string-start (nth 8 parse-state)))
    (and string-start (get-text-property string-start 'dhall-string-type))))

(defun dhall--mark-string (pos string-type)
  "Mark string as a Dhall string.

POS position of start of string
STRING-TYPE type of string based off of Emacs syntax table types"
  (put-text-property pos (1+ pos)
                     'syntax-table (string-to-syntax "|"))
  (put-text-property pos (1+ pos)
                     'dhall-string-type string-type))

(defun dhall--double-quotes ()
  "Handle Dhall double quotes."
  (let* ((pos (match-beginning 0))
          (ps (dhall--get-parse-state pos))
          (string-type (dhall--get-string-type ps)))
    (dhall--mark-string pos ?\")))

(defun dhall--single-quotes ()
  "Handle Dhall single quotes"
  (let* ((pos (match-beginning 0))
          (ps (dhall--get-parse-state pos))
          (string-type (dhall--get-string-type ps)))
    (dhall--mark-string pos ?\")))

(defun dhall-syntax-propertize (start end)
  "Special syntax properties for Dhall from START to END"
  (goto-char start)
  (remove-text-properties start end '(syntax-table nil dhall-string-type nil))
  (funcall
   (syntax-propertize-rules
    ("'\\{2,\\}"
     (0 (ignore (dhall--single-quotes))))
    ("\""
     (0 (ignore (dhall--double-quotes))))
    )start end))

;; The main mode functions
;;;###autoload
(define-derived-mode dhall-mode prog-mode 
  "Dhall"
  "Major mode for editing Dhall files." 
  :group 'dhall 
  :syntax-table dhall-mode-syntax-table
  (setq font-lock-defaults '(dhall-mode-font-lock-keywords))
  (setq-local indent-tabs-mode t) 
  (setq-local tab-width 4) 
  ;; Special syntax properties for Dhall
  (setq-local syntax-propertize-function 'dhall-syntax-propertize)
  
  ;; Look at text properties when parsing
  ;; (setq-local parse-sexp-lookup-properties t)
  (add-hook 'after-save-hook 'dhall-format-maybe nil t))

;; Automatically use dhall-mode for .dhall files.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dhall\\'" . dhall-mode))

;; Provide ourselves:
(provide 'dhall-mode)
;;; dhall-mode.el ends here
