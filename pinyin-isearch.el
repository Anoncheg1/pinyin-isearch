;;; pinyin-isearch.el --- isearch mode for Chinese toneless pinyin search.  -*- lexical-binding: t -*-

;; Copyright (c) 2023 Anoncheg1

;; Author: Anoncheg1
;; Keywords: chinese, pinyin, matching, convenience
;; URL: https://github.com/Anoncheg1/pinyin-isearch
;; Version: 1.5
;; Package-Requires: ((emacs "27.2"))

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
;; There is two types of search: for pinyin (pīnyīn) and for Chinese
;; characters (汉字) text.
;;
;; You can use both or select one of them.
;; Pinyin without tones is used for input.
;; Input is transformed to regex expression like:
;; "\\([嗯唔][爱哀挨埃癌]\\|[乃奶奈耐氖艿鼐柰]\\|n\\([ūúǔùǖǘǚǜ]\\s-*e\\|ü[ēéěè]\\)\\)"
;;
;; To activate:
;; add (require 'pinyin-isearch) line to your
;; ~/.emacs or ~/.emacs.d/init.el
;;
;; to activate isearch submodes add line: (pinyin-isearch-activate-submodes)
;;
;; To use:
;;
;; M-x pinyin-isearch-mode
;; C-u C-s for normal search.
;; or
;; C-s M-s p/h/s - to activate (p)inyin or (h)ieroglyph (s)trict
;; hieroglyph search submode.
;; or
;; M-x pinyin-isearch-forward/backward
;;
;; Configuration:
;;
;; M-x customize-group RET pinyin-isearch
;;
;; This package depends on Quail minor mode (input multilingual text
;; easily) and uses it's translation table (named Quail map).
;; It is possible to adopt this code to many other languages.

;;; Code:

(require 'pinyin-isearch-pinyin)
(require 'pinyin-isearch-hieroglyphs)

(declare-function pinyin-isearch-pinyin-regexp-function "pinyin-isearch-pinyin" (string &optional lax))
(declare-function pinyin-isearch-hieroglyphs-regexp-function "pinyin-isearch-hieroglyphs" (string &optional lax))
(declare-function pinyin-isearch-hieroglyphs-strict-regexp-function "pinyin-isearch-hieroglyphs" (string &optional lax))

(defgroup pinyin-isearch nil
  "Fuzzy Matching."
  :group 'pinyin-isearch
  :prefix "pinyin-isearch-")

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
                 (const :tag "Search in Chinese characters only" characters)
                 (const :tag "Search in Chinese characters only" t)
                 (const :tag "Search in pinyin only" pinyin)
                 (const :tag "Search in pinyin only" nil))
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



(defun pinyin-isearch-both-regexp-function (string &optional lax)
  "Replacement for function `isearch-regexp-function'.
Concat pinyin and Chinese characters regex as alternation.
Argument STRING is a query string.
Optional argument LAX for isearch special cases."
  (setq lax lax) ; suppers Warning: Unused lexical argument `lax'
  (let* ((psr (pinyin-isearch-pinyin-regexp-function string))
        (hsr (pinyin-isearch-hieroglyphs-regexp-function string))
        (p (string-prefix-p "\\(" psr))
        (h (string-prefix-p "\\(" hsr)))
    (cond
     ;; 1 1
     ((and p h)
      (concat "\\("
              (substring psr 3) ;; skip \\(
              "\\|"
              (substring hsr 0 -3) ;; skip \\)
              "\\)"))
     ;; 1 0
     ((and p (not h))
      (concat "\\("
              (substring psr 3) ;; skip \\(
              "\\|"
              hsr
              "\\)"))
     ;; 0 1
     ((and p (not h))
      (concat "\\("
              psr
              "\\|"
              (substring hsr 0 -3) ;; skip \\)
              "\\)"))
     ;; 0 0
     ((not (and p h))
      (concat "\\("
              psr
              "\\|"
              hsr
              "\\)")))
  ))


(defun pinyin-isearch--set-isearch()
  "Help subfunction to replace isearch functions.
Used in functions `pinyin-isearch-forward' and
`pinyin-isearch-backward'."
  (let ((func (cond
               ;; both
               ((eq pinyin-isearch-target 'both)
                     #'pinyin-isearch-both-regexp-function)
               ;; hieroglyphs
               ((or (eq pinyin-isearch-target 'hieroglyphs)
                    (eq pinyin-isearch-target t))
                #'pinyin-isearch-hieroglyphs-regexp-function)
               ;; pinyin
               ((or (eq pinyin-isearch-target 'pinyin)
                    (eq pinyin-isearch-target nil) )
                #'pinyin-isearch-pinyin-regexp-function))))
    (setq search-default-mode func)
    (setq isearch-regexp-function func)))


(defun pinyin-isearch--pinyin-fix-jumping-advice ()
  "Advice to fix isearch behavior.  Force search from a starting point."
  (if (and isearch-mode pinyin-isearch-fix-jumping-flag
           (or
           (eq isearch-regexp-function #'pinyin-isearch-pinyin-regexp-function)
           (eq isearch-regexp-function #'pinyin-isearch-hieroglyphs-regexp-function)
           (eq isearch-regexp-function #'pinyin-isearch-hieroglyphs-strict-regexp-function)
           (eq isearch-regexp-function #'pinyin-isearch-both-regexp-function)))
      (let ((key (this-single-command-keys)))
        ;; (print (lookup-key isearch-mode-map key nil) ) ; debug
        (when (and isearch-success
                   (eq 'isearch-printing-char (lookup-key isearch-mode-map key nil)))
          (goto-char isearch-opoint)
          (setq isearch-adjusted t)))))

;; ------------ interface with isearch and user --------------

;; used in all modes
(add-hook 'pre-command-hook 'pinyin-isearch--pinyin-fix-jumping-advice)

;;;###autoload
(defun pinyin-isearch-activate-submodes()
  "Add submodes to `isearch-mode' accessible with key `M-s KEY'.
Call macros to define global functions `isearch-toggle-*.'"
  (isearch-define-mode-toggle "pinyin" "p" pinyin-isearch-pinyin-regexp-function "\
  Turning on pinyin search turns off normal mode.")

  (isearch-define-mode-toggle "characters" "h" pinyin-isearch-hieroglyphs-regexp-function "\
  Turning on characters search turns off normal mode.")

  (isearch-define-mode-toggle "strict" "s" pinyin-isearch-hieroglyphs-strict-regexp-function "\
  Turning on strict characters search turns off normal mode.")

  (put 'pinyin-isearch-pinyin-regexp-function 'isearch-message-prefix (format "%s " "[Pinyin-P]"))

  (put 'pinyin-isearch-hieroglyphs-regexp-function 'isearch-message-prefix (format "%s " "[Pinyin-H]"))

  (put 'pinyin-isearch-hieroglyphs-strict-regexp-function 'isearch-message-prefix (format "%s " "[Pinyin-HS]")))


;;;###autoload
(defun pinyin-isearch-forward (&optional regexp-p no-recursive-edit)
  "Do incremental search forward.
Optional argument REGEXP-P see original function `isearch-forward'.
Optional argument NO-RECURSIVE-EDIT see original function `isearch-forward'."
  (interactive "P\np")
  ;;save
  (setq pinyin-isearch--original-search-default-mode search-default-mode)
  (setq pinyin-isearch--original-isearch-regexp-function isearch-regexp-function)
  ;; set
  (pinyin-isearch--set-isearch)

  ;; call interactively or simple
  (if (called-interactively-p "any")
          (funcall-interactively #'isearch-forward regexp-p no-recursive-edit)
        ;; else
        (apply #'isearch-forward '(regexp-p no-recursive-edit)))
  ;; restore
  (setq search-default-mode pinyin-isearch--original-search-default-mode)
  ;; (setq isearch-regexp-function pinyin-isearch--original-isearch-regexp-function) ; should not be restored
  )


;;;###autoload
(defun pinyin-isearch-backward (&optional regexp-p no-recursive-edit)
  "Do incremental search backward.
Optional argument REGEXP-P see original function `isearch-backward'.
Optional argument NO-RECURSIVE-EDIT see original function `isearch-backward'."
  (interactive "P\np")
  ;; save
  (setq pinyin-isearch--original-search-default-mode search-default-mode)
  (setq pinyin-isearch--original-isearch-regexp-function isearch-regexp-function)
  ;; set
  (pinyin-isearch--set-isearch)
  ;; call interactively or simple
  (if (called-interactively-p "any")
          (funcall-interactively #'isearch-backward regexp-p no-recursive-edit)
    ;; else
    (apply #'isearch-backward '(regexp-p no-recursive-edit)))
  ;; restore
  (setq search-default-mode pinyin-isearch--original-search-default-mode)
  ;; (setq isearch-regexp-function pinyin-isearch--original-isearch-regexp-function) ; should not be restored
)


;;;###autoload
(define-minor-mode pinyin-isearch-mode
  "Replace key bindings for functions `isearch-forward' and `isearch-backward'.
Allow with query {pinyin} to find {pīnyīn}.  \\C-\\u \\C-\\s used for
normal search."
  :lighter " p-isearch" :global nil :group 'isearch :version "27.2"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-s") #'pinyin-isearch-forward)
            (define-key map (kbd "C-r") #'pinyin-isearch-backward)
            map))


(provide 'pinyin-isearch)
;;; pinyin-isearch.el ends here
