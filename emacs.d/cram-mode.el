;;; cram-mode.el --- Emacs mode for CRAM tests            -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Kiran Gopinathan

;; Author: Kiran Gopinathan
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

;;; Imports & Declarations:

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.t$" . cram-mode))

;;; Font lock syntax highlighting:

(defvar cram--font-lock-defaults
  '((
     ("^  [^$>][^\n]*\n" . font-lock-comment-face)
     ("\\(^ ?\n\\|^ ?[^ \n][^\n]*\n\\)" . font-lock-doc-face)
     ;; ("\\(^  \\$[^\n]*\n\\|^  >[^\n]*\n\\)" . font-lock-type-face)
     ;; ("^[^\"]*\\(\"[^\"\n]*\"\\)[^\"]*$" . (1 font-lock-string-face))
     ("\"\\([^\"\n]\\|\\\"\\)*\"" . (0 font-lock-string-face))
     ("'\\([^'\n]\\|\\'\\)*'" . font-lock-string-face)
     ))
  ;; "Default highlighting expressions for cram mode"
  )

;;; Compilation functions:

(defun cram-mode-compile ()
  "Compile the project and run cram test."
  (interactive)
  (compile "CLICOLOR_FORCE=1 OCAML_COLORS=always dune runtest"))

(defun cram-mode-compile-and-promote ()
  "Compile the project and run cram test, promote results."
  (interactive)
  (compile "CLICOLOR_FORCE=1 OCAML_COLORS=always dune runtest --auto-promote"))

;;; Mode map:

(defvar cram-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "<f1>")
      #'cram-mode-compile)
    (define-key map (kbd "<f2>")
      #'cram-mode-compile-and-promote)
    map)
  "Keymap for cram major mode.")

;;; Mode declaration: 
;;;###autoload
(define-derived-mode cram-mode fundamental-mode "cram"
  "Major mode for cram tests."
  ;; :syntax-table cram-mode-syntax-table
  (setq font-lock-keywords-only t)
  (setq font-lock-defaults cram--font-lock-defaults)
  (modify-syntax-entry ?\" "w" cram-mode-syntax-table)
  (modify-syntax-entry ?_ "w" cram-mode-syntax-table)
  (modify-syntax-entry ?. "w" cram-mode-syntax-table)
  (auto-revert-mode)
  )

(provide 'cram-mode)
;;; cram-mode.el ends here
