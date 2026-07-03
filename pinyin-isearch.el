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
;; (pinyin-isearch-load) ; force loading (optional) before mode

;;;; Usage:

;; M-x pinyin-isearch-mode
;; C-u C-s for normal search.
;; or
;; C-s M-s p/h/s - to activate (p)inyin or (h) Chonese characters (s)trict
;; C-s M-s F1 - to see all keys

;; Usage without activation of minor mode is possible with:
;; M-x pinyin-isearch-forward/backward
;;
;; Customization:
;;
;; M-x customize-group RET pinyin-isearch
;;
;; This package depends on Quail minor mode (input multilingual text
;; easily) and uses it's translation table (named Quail map).
;; It is possible to adopt this code to many other languages.

;; Search variants:

;; By default active search for both pinyin and chinese characters
;;  with fallback to latin normal characters.

;;;; Other packages:

;; - Modern navigation in major modes https://github.com/Anoncheg1/firstly-search
;; - Ediff no 3-th window	https://github.com/Anoncheg1/ediffnw
;; - Dired history		https://github.com/Anoncheg1/dired-hist
;; - Selected window contrast	https://github.com/Anoncheg1/selected-window-contrast
;; - Copy link to clipboard	https://github.com/Anoncheg1/emacs-org-links
;; - Solution for "callback hell"	https://github.com/Anoncheg1/emacs-async1
;; - Restore buffer state	https://github.com/Anoncheg1/emacs-unmodified-buffer1
;; - outline.el usage		https://github.com/Anoncheg1/emacs-outline-it
;; - Dates for Org-mode headers	https://github.com/Anoncheg1/emacs-org-history
;; - Call LLMs and agents from Org-mode cui blocks https://github.com/Anoncheg1/emacs-cui

;;;; Donate:

;; - BTC (Bitcoin) address: 1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25
;; - USDT (Tether) address: TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN
;; - TON (Telegram) address: UQC8rjJFCHQkfdp7KmCkTZCb5dGzLFYe2TzsiZpfsnyTFt9D

;;; Code:
(require 'isearch)
(require 'cl-lib) ; (require 'cl-macs) ; Warning: the function ‘cl-callf’ is not known to be defined.
(require 'pinyin-isearch-pinyin)
(require 'pinyin-isearch-chars)

(declare-function pinyin-isearch-pinyin-regexp-function "pinyin-isearch-pinyin" (string &optional lax))
(declare-function pinyin-isearch-chars-regexp-function "pinyin-isearch-chars" (string &optional lax))
(declare-function pinyin-isearch-chars-strict-regexp-function "pinyin-isearch-chars" (string &optional lax))
(declare-function pinyin-isearch-chars-load "pinyin-isearch-chars")
(declare-function pinyin-isearch-pinyin-load "pinyin-isearch-pinyin")

(defgroup pinyin-isearch nil
  "Fuzzy Matching by pinyin."
  :group 'pinyin-isearch)

;; Todo, check pinyin completion
(defcustom pinyin-isearch-strict nil
  "Non-nil means prohibit adding to search all possible completion.
By default we are looking for all characters and pinyin syllables that
 start with typed by user first part of syllable."
  :local t
  :type 'boolean
  :group 'pinyin-isearch)

(defcustom pinyin-isearch-full-fallback t
  "Non-nil means search for normal normal latin letters also.
For mandaring characters, wif non-nil we always search for full latin
 input.
For pinyin search this means we use latin symbols in regex for vowels to
 search for letters without diacritical mark tones.  If is nil and
 syllable was not found pinyin search dont look for latin string.
Used in `pinyin-isearch-chars--concat-variants'."
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
                 (const :tag "Search in both: pinyin and Chinese characters strictly" strict)
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

(defun pinyin-isearch-both-regexp-function (string &optional _lax)
  "Concat pinyin and Chinese chars regex as alternation for isearch.
Main function.
Replacement for function `isearch-regexp-function'.
Argument STRING is a query string.
Optional argument LAX for isearch special cases."
  (let* ((psr (pinyin-isearch-pinyin-regexp-function string))
         (hsr (let ((pinyin-isearch-full-fallback nil))
                (pinyin-isearch-chars-regexp-function string))))
    (cond
     ((equal hsr "$^") psr)
     ((equal psr "$^") hsr)
     ((equal psr hsr) psr)
     (t
      (concat "\\(" psr "\\|" hsr "\\)")))))

(defun pinyin-isearch-both-strinct-regexp-function (string &optional lax)
  "Strict variant of `pinyin-isearch-both-regexp-function'.
Argument STRING is a query string, LAX is not used."
  (let ((pinyin-isearch-strict t)
        (pinyin-isearch-chars-fallback nil)
        (pinyin-isearch-full-fallback nil))
    (pinyin-isearch-both-regexp-function string lax)))

(defun pinyin-isearch--set-isearch ()
  "Help subfunction to replace isearch functions.
Used in functions `pinyin-isearch-forward' and
`pinyin-isearch-backward'."
  (cond
   ;; both
   ((eq pinyin-isearch-target 'both)
    #'pinyin-isearch-both-regexp-function)
   ;; both strictly
   ((eq pinyin-isearch-target 'strict)
    #'pinyin-isearch-both-strinct-regexp-function)
   ;; hieroglyphs
   ((or (eq pinyin-isearch-target 'characters)
        (eq pinyin-isearch-target t))
    #'pinyin-isearch-chars-regexp-function)
   ;; pinyin
   ((or (eq pinyin-isearch-target 'pinyin)
        (not pinyin-isearch-target))
    #'pinyin-isearch-pinyin-regexp-function)))

(defun pinyin-isearch--pinyin-fix-jumping-advice ()
  "Advice to fix isearch behavior.  Force search from a starting point."
  (if (and isearch-mode pinyin-isearch-fix-jumping-flag
           (or
            (eq isearch-regexp-function #'pinyin-isearch-pinyin-regexp-function)
            (eq isearch-regexp-function #'pinyin-isearch-chars-regexp-function)
            (eq isearch-regexp-function #'pinyin-isearch-chars-strict-regexp-function) ; TODO
            (eq isearch-regexp-function #'pinyin-isearch-both-regexp-function)))
      (let ((key (this-single-command-keys)))
        (when (and isearch-success
                   (eq #'isearch-printing-char (lookup-key isearch-mode-map key nil)))
          (goto-char isearch-opoint)
          (setq isearch-adjusted t)))))

(defun pinyin-isearch-load ()
  "Load and activate modules and hooks used by all."
  (unless (and pinyin-isearch-chars--first-syllable-letters ; for speed
               pinyin-isearch-pinyin-syllable-table)
    (pinyin-isearch-chars-load) ; activate pinyin-isearch-chars
    (pinyin-isearch-pinyin-load))) ; activate pinyin-isearch-pinyin


;; -=-= ------------ interface with isearch and user --------------

;;;###autoload
(defun pinyin-isearch-forward (&optional regexp-p no-recursive-edit)
  "Do incremental search forward.
If called with a universal argument (C-u), falls back to standard
 `isearch-forward'.
Optional argument REGEXP-P see original function `isearch-forward'.
Optional argument NO-RECURSIVE-EDIT see original function
 `isearch-forward'."
  (interactive "P\np")
  (if current-prefix-arg
      ;; If C-u was pressed, run standard isearch
      (isearch-forward regexp-p no-recursive-edit)
    ;; Otherwise, run your custom pinyin isearch
    (pinyin-isearch-load) ; lazy loading
    (isearch-mode t regexp-p nil (not no-recursive-edit) (pinyin-isearch--set-isearch))))

;;;###autoload
(defun pinyin-isearch-backward (&optional regexp-p no-recursive-edit)
  "Do incremental search backward.
If called with a universal argument (C-u), falls back to standard
 `isearch-backward'.
Optional argument REGEXP-P see original function `isearch-backward'.
Optional argument NO-RECURSIVE-EDIT see original function `isearch-backward'."
  (interactive "P\np")
  (if current-prefix-arg
      (isearch-backward regexp-p no-recursive-edit)
    ;; else
    (pinyin-isearch-load) ; lazy loading, for usage without minor mode
    (isearch-mode nil regexp-p nil (not no-recursive-edit) (pinyin-isearch--set-isearch))))

;; Generate the toggles globally (Emacs automatically binds them to M-s p, M-s h, M-s s)
;; Turns off normal mode.
(isearch-define-mode-toggle "both" "n" pinyin-isearch-both-regexp-function
  "Turning on default search for pinyin and characters.")
(isearch-define-mode-toggle "pinyin" "p" pinyin-isearch-pinyin-regexp-function
  "Turning on pinyin search (P).")
(isearch-define-mode-toggle "characters" "h" pinyin-isearch-chars-regexp-function
  "Turning on characters search (H).")
(isearch-define-mode-toggle "strict-both" "s" pinyin-isearch-both-strinct-regexp-function
  "Turning on strict pinyin and characters search (S).")
(isearch-define-mode-toggle "strict-characters" "u" pinyin-isearch-chars-strict-regexp-function
  "Turning on strict characters search (HS).")

(put #'pinyin-isearch-both-regexp-function #'isearch-message-prefix "[Pinyin] ")
(put #'pinyin-isearch-pinyin-regexp-function #'isearch-message-prefix "[Pinyin-P] ")
(put #'pinyin-isearch-chars-regexp-function #'isearch-message-prefix "[Pinyin-H] ")
(put #'pinyin-isearch-both-strinct-regexp-function #'isearch-message-prefix "[Pinyin-S] ")
(put #'pinyin-isearch-chars-strict-regexp-function #'isearch-message-prefix "[Pinyin-HS] ")

(defvar pinyin-isearch-m-s-map
  (let ((map (make-sparse-keymap)))
    ;; Explicitly bind the newly created pinyin toggles to YOUR map
    ;; This guarantees they show up when a user hits C-s M-s F1
    (define-key map (kbd "n") #'isearch-toggle-both)
    (define-key map (kbd "p") #'isearch-toggle-pinyin)
    (define-key map (kbd "h") #'isearch-toggle-characters)
    (define-key map (kbd "s") #'isearch-toggle-strict-both)
    (define-key map (kbd "u") #'isearch-toggle-strict-characters)

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
        (pinyin-isearch-load)
        ;; used in all modes
        (add-hook 'pre-command-hook #'pinyin-isearch--pinyin-fix-jumping-advice nil t))
    ;; else
    (remove-hook 'pre-command-hook #'pinyin-isearch--pinyin-fix-jumping-advice t)))

;; -=-= provide
(provide 'pinyin-isearch)
;;; pinyin-isearch.el ends here
