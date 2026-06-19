;;; pinyin-isearch.el --- Pinyin mode for isearch  -*- lexical-binding: t -*-

;; Copyright (c) 2024 github.com/Anoncheg1,codeberg.org/Anoncheg

;; Author: github.com/Anoncheg1,codeberg.org/Anoncheg
;; Maintainer: github.com/Anoncheg1,codeberg.org/Anoncheg
;; Keywords: chinese, pinyin, matching, convenience
;; URL: https://github.com/Anoncheg1/pinyin-isearch
;; Version: 1.6.9
;; Package-Requires: ((emacs "28.1"))
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; License

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; There are two types of search: for pinyin (pīnyīn) and for Chinese
;; characters (汉字) text.

;; You can use both or select one of them.
;; Pinyin without tones is used for input.
;; Input is transformed to regex expression like:
;; "\\([嗯唔][爱哀挨埃癌]\\|[乃奶奈耐氖艿鼐柰]\\|n\\([ūúǔùǖǘǚǜ]\\s-*e\\|ü[ēéěè]\\)\\)"

;; Configuration in ~/.emacs or ~/.emacs.d/init.el:

;; (require 'pinyin-isearch)
;; (pinyin-isearch--activate) ; force loading (optional) before mode

;;;; Usage:

;; M-x pinyin-isearch-mode
;; C-u C-s for normal search.
;; or
;; C-s M-s p/h/s - to activate (p)inyin or (h) Chonese characters (s)trict
;; Chinese characters search submode.
;; or
;; M-x pinyin-isearch-forward/backward
;;
;; Customization:
;;
;; M-x customize-group RET pinyin-isearch
;;
;; This package depends on Quail minor mode (input multilingual text
;; easily) and uses it's translation table (named Quail map).
;; It is possible to adopt this code to many other languages.

;;; Code:

(require 'cl-lib); (require 'cl-macs) ; Warning: the function ‘cl-callf’ is not known to be defined.
(require 'pinyin-isearch-pinyin)
(require 'pinyin-isearch-chars)

(declare-function pinyin-isearch-pinyin-regexp-function "pinyin-isearch-pinyin" (string &optional lax))
(declare-function pinyin-isearch-chars-regexp-function "pinyin-isearch-chars" (string &optional lax))
(declare-function pinyin-isearch-chars-strict-regexp-function "pinyin-isearch-chars" (string &optional lax))
(declare-function pinyin-isearch-chars-load "pinyin-isearch-chars")
(declare-function pinyin-isearch-pinyin-load "pinyin-isearch-pinyin")
;; (declare-function isearch-toggle-strict "pinyin-isearch")
;; (declare-function isearch-toggle-characters "pinyin-isearch")
;; (declare-function isearch-toggle-pinyin "pinyin-isearch")

(defgroup pinyin-isearch nil
  "Fuzzy Matching by pinyin."
  :group 'pinyin-isearch)

(defcustom pinyin-isearch-strict nil
  "Non-nil means Enforce to search only pinyin and Chinese characters.
isearch will not fallback to find normal latin text if pinyin was
not found.  Configure `pinyin-isearch-mode' and pinyin isearch
submode also."
  :local t
  :type 'boolean
  :group 'pinyin-isearch)

(defcustom pinyin-isearch-target 'both
  "Select target text in `pinyin-isearch-mode'.
Whether to search for for pinyin or for Chinese characters or for
both of them.  Used for mode `pinyin-isearch-mode', and functions
`pinyin-isearch-forward', `pinyin-isearch-backward'."
  :local t
  :type '(choice (const :tag "Search in both: pinyin and Chinese characters" both)
                 (const :tag "Search in Chinese characters only, same as t" characters)
                 (const :tag "Search in Chinese characters only, same as characters" t)
                 (const :tag "Search in pinyin only, same as nil" pinyin)
                 (const :tag "Search in pinyin only, same as pinyin" nil))
  :group 'pinyin-isearch)

(defcustom pinyin-isearch-fix-jumping-flag t
  "Non-nil means fix isearch behavior.
When typing new character the new search begins from last
success found occurance, not from when you begin whole search.
This fix force isearch to begin from the starting point.
Disable for native isearch behavior."
  :local t
  :type 'boolean
  :group 'pinyin-isearch)

(defvar-local pinyin-isearch--original-search-default-mode search-default-mode
  "Used in `pinyin-isearch--set-isearch' to save previous state.")

(defvar-local pinyin-isearch--original-isearch-regexp-function isearch-regexp-function
  "Used in `pinyin-isearch--set-isearch' to save previous state.")

;; -=-= fns

;; (defun pinyin-isearch-both-regexp-function (string &optional lax)
;;   "Replacement for function `isearch-regexp-function'.
;; Concat pinyin and Chinese characters regex as alternation.
;; Argument STRING is a query string.
;; Optional argument LAX for isearch special cases."
;;   (setq lax lax) ; suppers Warning: Unused lexical argument `lax'
;;   (let* ((psr (pinyin-isearch-pinyin-regexp-function string))
;;         (hsr (pinyin-isearch-chars-regexp-function string))
;;         (p (string-prefix-p "\\(" psr))
;;         (h (string-prefix-p "\\(" hsr)))
;;     (cond
;;      ;; 1 nil
;;      ((equal hsr "$^") psr)
;;      ;; nil 1
;;      ((equal psr "$^") hsr)
;;      ;; ? == ?
;;      ((equal psr hsr) psr)
;;      ;; 1 1
;;      ((and p h)
;;       (concat (substring psr 0 -3) ;; skip \\)
;;               "\\|"
;;               (substring hsr 3) ;; skip \\(
;;               ))
;;      ;; 1 0
;;      ((and p (not h))
;;       (concat (substring psr 0 -3) ;; skip \\)
;;               "\\|"
;;               hsr
;;               "\\)"))
;;      ;; 0 1
;;      ((and p (not h))
;;       (concat "\\("
;;               psr
;;               "\\|"
;;               (substring hsr 3) ;; skip \\(
;;               "\\)"))
;;      ;; 0 0
;;      ((not (and p h))
;;       (concat "\\("
;;               psr
;;               "\\|"
;;               hsr
;;               "\\)")))))

(defun pinyin-isearch-both-regexp-function (string &optional lax)
  "Concat pinyin and Chinese chars regex as alternation for isearch.
Replacement for function `isearch-regexp-function'.
Argument STRING is a query string.
Optional argument LAX for isearch special cases."
  (ignore lax)
  (let* ((psr (pinyin-isearch-pinyin-regexp-function string))
         (hsr (pinyin-isearch-chars-regexp-function string)))
    (cond
     ((equal hsr "$^") psr)
     ((equal psr "$^") hsr)
     ((equal psr hsr) psr)
     (t
      (concat "\\(" psr "\\|" hsr "\\)")))))

(defun pinyin-isearch--set-isearch ()
  "Help subfunction to replace isearch functions.
Used in functions `pinyin-isearch-forward' and
`pinyin-isearch-backward'."
  (cond
   ;; both
   ((eq pinyin-isearch-target 'both)
         #'pinyin-isearch-both-regexp-function)
   ;; hieroglyphs
   ((or (eq pinyin-isearch-target 'characters)
        (eq pinyin-isearch-target t))
    #'pinyin-isearch-chars-regexp-function)
   ;; pinyin
   ((or (eq pinyin-isearch-target 'pinyin)
        (not pinyin-isearch-target) )
    #'pinyin-isearch-pinyin-regexp-function)))

(defun pinyin-isearch--pinyin-fix-jumping-advice ()
  "Advice to fix isearch behavior.  Force search from a starting point."
  (if (and isearch-mode pinyin-isearch-fix-jumping-flag
           (or
            (eq isearch-regexp-function #'pinyin-isearch-pinyin-regexp-function)
            (eq isearch-regexp-function #'pinyin-isearch-chars-regexp-function)
            (eq isearch-regexp-function #'pinyin-isearch-chars-strict-regexp-function)
            (eq isearch-regexp-function #'pinyin-isearch-both-regexp-function)))
      (let ((key (this-single-command-keys)))
        ;; (print (lookup-key isearch-mode-map key nil) ) ; debug
        (when (and isearch-success
                   (eq #'isearch-printing-char (lookup-key isearch-mode-map key nil)))
          (goto-char isearch-opoint)
          (setq isearch-adjusted t)))))

(defun pinyin-isearch--activate ()
  "Load and activate modules and hooks used by all."
  (pinyin-isearch-chars-load) ; activate pinyin-isearch-chars
  (pinyin-isearch-pinyin-load) ; activate pinyin-isearch-pinyin
  ;; used in all modes
  ;; (add-hook 'pre-command-hook #'pinyin-isearch--pinyin-fix-jumping-advice)
  )

;; -=-= ------------ interface with isearch and user --------------

;; ;;;###autoload
;; (defun pinyin-isearch-activate-submodes ()
;;   "Add submodes to `isearch-mode' accessible with key `M-s KEY'.
;; Call macros to define global functions `isearch-toggle-*.'"
;;   (pinyin-isearch--activate)

;;   (isearch-define-mode-toggle "pinyin" "p" pinyin-isearch-pinyin-regexp-function "\
;;   Turning on pinyin search turns off normal mode.")

;;   (isearch-define-mode-toggle "characters" "h" pinyin-isearch-chars-regexp-function "\
;;   Turning on characters search turns off normal mode.")

;;   (isearch-define-mode-toggle "strict" "s" pinyin-isearch-chars-strict-regexp-function "\
;;   Turning on strict characters search turns off normal mode.")

;;   (put #'pinyin-isearch-pinyin-regexp-function #'isearch-message-prefix (format "%s " "[Pinyin-P]"))

;;   (put #'pinyin-isearch-chars-regexp-function #'isearch-message-prefix (format "%s " "[Pinyin-H]"))

;;   (put #'pinyin-isearch-chars-strict-regexp-function #'isearch-message-prefix (format "%s " "[Pinyin-HS]")))


;; ;;;###autoload
(defun pinyin-isearch-forward (&optional regexp-p no-recursive-edit)
  "Do incremental search forward.
Optional argument REGEXP-P see original function `isearch-forward'.
Optional argument NO-RECURSIVE-EDIT see original function `isearch-forward'."
  (interactive "P\np")
  ;; (pinyin-isearch--activate)
  (isearch-mode t regexp-p nil (not no-recursive-edit) (pinyin-isearch--set-isearch)))

(defun pinyin-isearch-backward (&optional regexp-p no-recursive-edit)
  "Do incremental search backward.
Optional argument REGEXP-P see original function `isearch-backward'.
Optional argument NO-RECURSIVE-EDIT see original function `isearch-backward'."
  (interactive "P\np")
  ;; (pinyin-isearch--activate)
  (isearch-mode nil regexp-p nil (not no-recursive-edit) (pinyin-isearch--set-isearch)))

;; 1. Generate the toggles globally (Emacs automatically binds them to M-s p, M-s h, M-s s)
(isearch-define-mode-toggle "pinyin" "p" pinyin-isearch-pinyin-regexp-function
  "Turning on pinyin search turns off normal mode.")
(isearch-define-mode-toggle "characters" "h" pinyin-isearch-chars-regexp-function
  "Turning on characters search turns off normal mode.")
(isearch-define-mode-toggle "strict" "s" pinyin-isearch-chars-strict-regexp-function
  "Turning on strict characters search turns off normal mode.")

(put #'pinyin-isearch-pinyin-regexp-function #'isearch-message-prefix "[Pinyin-P] ")
(put #'pinyin-isearch-chars-regexp-function #'isearch-message-prefix "[Pinyin-H] ")
(put #'pinyin-isearch-chars-strict-regexp-function #'isearch-message-prefix "[Pinyin-HS] ")

(defvar pinyin-isearch-m-s-map
  (let ((map (make-sparse-keymap)))
    ;; Bind your custom minor mode commands
    ;; (define-key map (kbd "o") #'my-custom-isearch-other-command)

    ;; Explicitly bind the newly created pinyin toggles to YOUR map
    ;; This guarantees they show up when a user hits C-s M-s F1
    (define-key map (kbd "p") #'isearch-toggle-pinyin)
    (define-key map (kbd "h") #'isearch-toggle-characters)
    (define-key map (kbd "s") #'isearch-toggle-strict)

    ;; Inherit from the standard isearch-mode M-s prefix map
    ;; This ensures standard options (like M-s _ for symbol search) still fall back correctly
    (set-keymap-parent map (lookup-key isearch-mode-map (kbd "M-s")))
    map)
  "Keymap for \\M -s bindings inside `pinyin-isearch-mode'.")


;;;###autoload
(define-minor-mode pinyin-isearch-mode
  "Replace key bindings for functions `isearch-forward' and `isearch-backward'.
Allow with query {pinyin} to find {pīnyīn}.  \\C-\\u \\C-\\s used for
normal search.
- M -s p `isearch-toggle-pinyin' to activate pinyin isearch submode.
- M -s h `isearch-toggle-characters' to activate Chinese characters
 isearch submode.
- M -s s `isearch-toggle-strict' to activate Chinese and pinying
 characters isearch submode.
- M -s r to activate standard search."
  :lighter " p-isearch" :global nil :group 'isearch
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-s") #'pinyin-isearch-forward)
            (define-key map (kbd "C-r") #'pinyin-isearch-backward)
            (define-key map (kbd "M-s") pinyin-isearch-m-s-map)
            map)
  ;; Manage the hooks cleanly
  (if pinyin-isearch-mode
      (progn
        (pinyin-isearch-chars-load) ; activate pinyin-isearch-chars
        (pinyin-isearch-pinyin-load) ; activate pinyin-isearch-pinyin
        ;; used in all modes
        (add-hook 'pre-command-hook #'pinyin-isearch--pinyin-fix-jumping-advice nil t))
    ;; else
    (remove-hook 'pre-command-hook #'pinyin-isearch--pinyin-fix-jumping-advice t)))

;; -=-= provide
(provide 'pinyin-isearch)
;;; pinyin-isearch.el ends here
