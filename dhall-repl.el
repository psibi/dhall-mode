;;; dhall-repl.el --- Dhall repl -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Patrick D. Elliott

;; Author: Patrick D. Elliott <patrick.d.elliott@gmail.com>
;; Keywords: Languages

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

;; A simple repl for dhall based on comint

;;; Code:

(defvar dhall-prompt-regexp "⊢ ")

(require 'comint)

(defcustom dhall-repl-executable "dhall-repl"
  "Location of dhall-repl command."
:type 'string)

(define-derived-mode dhall-repl-mode comint-mode "Dhall-REPL"
  "Interactive prompt for Dhall."
  (setq-local comint-prompt-regexp dhall-prompt-regexp)
(setq-local comint-prompt-read-only t))

(defun dhall-repl-show ()
  "Load the Dhall-REPL."
  (interactive)
  (pop-to-buffer-same-window
   (get-buffer-create "*Dhall-REPL*"))
  (unless (comint-check-proc (current-buffer))
    (dhall--make-repl-in-buffer (current-buffer))
(dhall-repl-mode)))

(defun dhall--make-repl-in-buffer (buffer)
  "Make Dhall Repl in BUFFER."
  (apply
   'make-comint-in-buffer `("Dhall-REPL" ,buffer ,dhall-repl-executable nil)))

(provide 'dhall-repl)
;;; dhall-repl.el ends here
