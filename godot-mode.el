;;; godot-mode.el --- Major mode for editing godot  files.  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Matthew Carter <m@ahungry.com>

;; Author: Matthew Carter <m@ahungry.com>
;; Maintainer: Matthew Carter <m@ahungry.com>
;; URL: https://github.com/ahungry/godot-mode
;; Version: 0.0.1
;; Date: 2018-02-15
;; Keywords: languages, godot,
;; Package-Requires: ((emacs "25.1") (cl-lib "0.6.1"))

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The dart-mode that exists doesn't properly indent, so give this a try.
;;
;; Much of this mode is derived from the js.el package.
;;
;; General Remarks:
;;
;; XXX: This mode assumes that block comments are not nested inside block
;; XXX: comments
;;
;; Exported names start with "godot-"; private names start with
;; "godot--".

;;; Code:

(require 'cl-lib)

(defvar godot-mode-default-tab-width 2)

(defconst godot-mode-font-lock-keywords-1
  (list
   '("var \\(.*?\\) =" 1 font-lock-variable-name-face)
   '("[\\.[:space:]{($]\\(_*[[:upper:]]+[[:upper:][:lower:]_$0-9]*\\)" 1 font-lock-type-face)
   '("\\(func\\) \\(.*?\\)(" 2 font-lock-function-name-face)
   '("\\(var\\|func\\)" . font-lock-keyword-face)
   '("\\(extends\\|if\\|self\\|else\\| or\\)" 1 font-lock-keyword-face)
   '("\\(\".*?\"\\)" . font-lock-string-face)
   '("\\('.*?'\\)" . font-lock-string-face)
 ))

(defvar default-tab-width 2)

;; This is easy because the syntax just cascades the indent until it
;; resets to 0 again.  No need to go back one level at a time etc.
(defun godot-indent-line ()
  "Properly indent based on what we're looking at."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t)
          (cur-indent (current-indentation)))
      (if (looking-at "^[ \t]*\\(var\\|func\\).*[:=]")
          (setq cur-indent 0)
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at "^[ \t]*\\(func\\|if\\|else\\).*:")
                (progn
                  (setq cur-indent (+ default-tab-width (current-indentation)))
                  (setq not-indented nil))
              (if (looking-at "^[ \t]*$")
                  (progn
                    (setq not-indented nil))
                (if (bobp) (setq not-indented nil))))))
        )
      (when (< cur-indent 0) (setq cur-indent 0))
      (if cur-indent (indent-line-to cur-indent) (indent-line-to 0))))
  )

;;;###autoload
(define-derived-mode godot-mode text-mode "Godot" ()
  "Major mode for editing  (Godot) files."
  :group 'languages
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'indent-line-function) 'godot-indent-line)
  (set (make-local-variable 'font-lock-defaults)
       '(godot-mode-font-lock-keywords-1)))

;;;###autoload
(defun godot-config ()
  "Default godot setup and bindings."
  (interactive)
  (add-to-list 'auto-mode-alist '("\\.gd\\'" . godot-mode)))

(provide 'godot-mode)

;;; godot-mode.el ends here
