;;; pinyin-isearch.el --- isearch mode for Chinese pinyin search.  -*- lexical-binding: t -*-

;; Copyright (c) 2023 Anoncheg1

;; Author: Anoncheg1
;; Keywords: chinese, pinyin, matching, convenience
;; URL: https://github.com/Anoncheg1/pinyin-isearch
;; Version: 1.3
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

;; Allow to search with Chinese pinyin in pinyin text and ignore
;; diacritical tone marks for speed.
;; Features:
;; - white spaces are ignored between syllables,
;;   but not ignored if in query
;; - tone required in text only for first syllable: Zhēn de ma
;; - should not conflict with other isearch modes
;; - search do not jump down but always begins from start point.
;; To activate use:
;; (require 'pinyin-isearch)
;; M-x pinyin-isearch-mode
;; or
;; M-x pinyin-isearch-forward / pinyin-isearch-backward
;; C-u C-s for normal search

;; How it works:
;; 1) we create list of ((\"zhuo\" . \"zhuō\")...) :
;;      pinyin-isearch-syllable-table
;; 2) we define isearch-toggle-[pinyin] with
;;      isearch-define-mode-toggle macros
;; 3) we find first longest syllable and very accurate do regex
;;      with tones "n\\([ūúǔùǖǘǚǜ]e\\|ü[ēéěè]\\)" for the rest of
;;      the line we apply rough regex for every vowel [eēéěè]

;;; Code:


;; I was unable to determinate reason for this error
;; It occure only during loading and use case sensitivity in search.
;; used sisheng-regexp, sisheng-vowel-table and sisheng-syllable-table.

(condition-case nil
    (load "quail/sisheng") ; (quail-use-package "chinese-sisheng" "quail/sisheng")
  (args-out-of-range nil))

(defvar sisheng-regexp :docstring "Located in quail/sisheng.")

(defvar sisheng-vowel-table :docstring "Located in quail/sisheng.")

(defvar sisheng-syllable-table :docstring "Located in quail/sisheng.")

;; -------------- after sisheng loading ----------

(defgroup pinyin-isearch nil
  "Fuzzy Matching."
  :group 'pinyin-isearch
  :prefix "pinyin-isearch-")

;; (defcustom pinyin-isearch-fix-jumping-flag t
;;   "Non-nil means fix isearch behavior.
;; When typing new character the new search begins from last
;; success found occurance, not from when you begin whole search.
;; This fix force isearch to begin from the starting point.
;; Disable for native isearch behavior."
;;   :type 'boolean
;;   :group 'pinyin-isearch)

;; (defcustom pinyin-isearch-fix-edit-flag t
;;   "Non-nil means fix isearch behavior.
;; After exiting isearch edit string, finction
;; `isearch-edit-string', isearch restart itself and forgot about
;; any modifications, such as this package.
;; Disable if you faced any issues."
;;   :type 'boolean
;;   :group 'pinyin-isearch)

(defcustom pinyin-isearch-strict nil
  "Non-nil means Enforce to search only pinyin.
isearch will not fallback to find normal latin text if pinyin is
not found.  Not this apply to for the first syllable only."
  :type 'boolean
  :group 'pinyin-isearch)

(defconst pinyin-isearch-vowel-table
  '(("a" "[āáǎà]")
    ("e" "[ēéěè]")
    ("i" "[īíǐì]")
    ("o" "[ōóǒò]")
    ("u" "[ūúǔùǖǘǚǜ]")
    ;; ("v" "[ūúǔùǖǘǚǜ]")
    ("ue" "ü[ēéěè]")
    ;; ("ve" "ü[ēéěè]")
))

(defconst pinyin-isearch-vowel-table-normal
  '(("a" "[aāáǎà]")
    ("e" "[eēéěè]")
    ("i" "[iīíǐì]")
    ("o" "[oōóǒò]")
    ("u" "[uūúǔùǖǘǚǜ]")
    ("ue" "[uü][eēéěè]")
    ;; ("ve" "[uü][eēéěè]")
))

(defconst pinyin-isearch-vowels
  '(("ā" "a")
    ("ē" "e")
    ("ī" "i")
    ("ō" "o")
    ("ū" "u")
    ("ǖ" "u")
    ("üē" "ue"))
    "Used to convert sisheng pinyin to toneless pinyin.")

;; (defconst pinyin-isearch-message-prefix "[Pinyin] "
;;   "Prepended to the isearch prompt when Pinyin searching is activated.")


(defun pinyin-isearch--sisheng-to-normal (syllable)
  "Convert \"zhuō\" 'SYLLABLE' to \"zhuo\".
Used to create list pinyin-isearch-syllable-table from original
sisheng."
  (string-match sisheng-regexp syllable)
  (let* (;; fin vowel
        (vowel-match (downcase (match-string 0 syllable)))
        (vowel-list (cdr (assoc-string vowel-match sisheng-vowel-table)))
        (base-key (nth 0 vowel-list))
        (base-key (if (equal base-key "v") "u"
                     ;; else
                     (if (equal base-key "ve") "ue" base-key)))
        )
    ;; fix for sisheng, we don't need "v"
    (replace-match base-key nil nil syllable)))


(defconst pinyin-isearch-syllable-table
    (mapcar (lambda (arg)
              (cons (pinyin-isearch--sisheng-to-normal arg) arg))
            sisheng-syllable-table) ;; sequence
    "Initialize syllable's table ((\"zhuo\" . \"zhuō\")...).")


(defun pinyin-isearch--get_vowel_from_sisheng (string)
       "For input \"zuō\" get \"o\".
Uses: constant `pinyin-isearch-vowels'.
Argument STRING sisheng syllable."
  (string-match sisheng-regexp string)
  (let* (
         (vowel-match (downcase (match-string 0 string)))
         (vowel-list
          (cdr (assoc-string vowel-match pinyin-isearch-vowels))))
    (car vowel-list)))


(defun pinyin-isearch--get-position-first-syllable(string)
  "Get position of the first syllable in query STRING.
It also return all vowels for all possible sub-syllables.
For \"zuom\" return (3 \"u\" \"o\").
Syllables with same tone vowel is ignored and used shortest.
Uses: function `pinyin-isearch--get_vowel_from_sisheng'
and global variable `pinyin-isearch-syllable-table'."
  (let (
        (first-chars)
        (pos (length string))
        (ret)
        (vowels nil)
        (syl))
    (while (> pos 0)
      ;; cut first chars
      (setq first-chars (substring string 0 pos))
      ;; find syllable in table
      (setq syl (cdr (assoc first-chars pinyin-isearch-syllable-table)))
      ;; if syllable found, add vowel with tone to vowels
      (if syl
          (let ((new-vow (pinyin-isearch--get_vowel_from_sisheng syl)))
            (if (not (member new-vow vowels))
                (setq vowels (cons (pinyin-isearch--get_vowel_from_sisheng syl) vowels))
              ;; else ignore previous long syllable with same vowel
              (setq ret nil))))
      ;; save position of the first longest syllable
      (if (and (null ret) syl)
          (setq ret pos))
      (setq pos (1- pos)))
    ;; -- let:
    (cons ret vowels)))


(defun pinyin-isearch--vowels-to-regex (vowels)
  "Used for accurate apply regex to the first syllable of toneless pinyin.
Convert (u o) to
\"\\([ūúǔùǖǘǚǜ][oōóǒò]\\|[uūúǔùǖǘǚǜ][ōóǒò]\\)\"
and (u) to \"[ūúǔùǖǘǚǜ]\".  Uses tables:
`pinyin-isearch-vowel-table',
`pinyin-isearch-vowel-table-normal'.  Argument VOWELS list of
normal vowels."
  (if (eq (length vowels) 2)
      (if  (not (member "ue" vowels))
          (let* ((pin-vowels (mapcar (lambda (x)
                                       (assoc-string x pinyin-isearch-vowel-table)) vowels))
                 (pin-vowels1 (car (cdr (car pin-vowels))))
                 (pin-vowels2 (car (cdr (car (cdr pin-vowels)))))
                 )
            (concat "\\(" pin-vowels1 "\\s-*" (car (cdr vowels))
                    "\\|" (car vowels) pin-vowels2 "\\)"))
        ;; ("u" "ue") case
        ;; (print (cdr (assoc-string "ue" pinyin-isearch-vowel-table)))
        (let ((p1  (concat (car (cdr (assoc-string "u" pinyin-isearch-vowel-table))) "\\s-*e")) ; [ūúǔùǖǘǚǜ]e
              (p2 (car (cdr (assoc-string "ue" pinyin-isearch-vowel-table))))  ; "ü[ēéěè]"
              )
          (concat "\\(" p1 "\\|" p2 "\\)") ; "\\([ūúǔùǖǘǚǜ]e\\|ü[ēéěè]\\)"
          ))
    ;; else if one vowel
    (car (cdr (assoc-string (car vowels) pinyin-isearch-vowel-table)))))


(defun pinyin-isearch--make-syllable-to-regex (syllable d-vowels)
  "Convert SYLLABLE \"zhuo\" to \"zh([]\|[])\".
Applyed to the first syllable to create accurate regex.
Uses function `pinyin-isearch--vowels-to-regex'.
Argument D-VOWELS result of function
 `pinyin-isearch--get-position-first-syllable'."
  (if (not (car d-vowels))
      syllable
    ;; else
    (let ((vowels-conc (if (member "ue" d-vowels) "ue"
                         ;; else
                         (apply #'concat (cdr d-vowels)))) ; "uo"
          (replacement (pinyin-isearch--vowels-to-regex (cdr d-vowels))))
      (string-replace vowels-conc replacement syllable))))


(defun pinyin-isearch--brute-replace (st &optional &key normal)
  "Replace every vowels in 'ST' with wide range regex.
if 'NORMAL' add normal to regex."
  (let* (
         ;; ignore white spaces if query is more than 2 characters
         (st (if (> (length st) 1)
                 ;; then - insert whitespaces between every character
                 (concat (substring st 0 1)
                         (mapconcat (lambda (x) (concat "\\s-*" x))
                                    (cdr (split-string st "" t))))
               ;; else
               st)))
    ;; ignore tones, based on lisp/leim/quail/sisheng.el
    (if normal
        (dolist ( c (split-string "aeiou" "" t))
          (let ((vowel-list-regex
                 (car (cdr (assoc-string c pinyin-isearch-vowel-table-normal))) ))
            (setq st (string-replace c vowel-list-regex st))))
      ;; else (not used now)
      (dolist ( c (split-string "aeiou" "" t))
          (let ((vowel-list-regex
                 (car (cdr (assoc-string c pinyin-isearch-vowel-table))) ))
            (setq st (string-replace c vowel-list-regex st)))))
    st))


(defun pinyin-isearch-regexp-function (string &optional lax)
  "Main function to convert query 'STRING' to regex for isearch.
Uses functions: `pinyin-isearch--get-position-first-syllable',
`pinyin-isearch--make-syllable-to-regex',
`pinyin-isearch--brute-replace'.
Optional argument LAX not used."
  (setq lax lax) ; suppers Warning: Unused lexical argument `lax'
  (let* ((st (regexp-quote string))
         ;; save length
         (len (length st))
         ;; get the first longest syllable
         (first-syllable-stat (if (> (length st) 1)
                                  (pinyin-isearch--get-position-first-syllable st)
                                ;; else
                                '(nil)))
         (first-syllable-pos (car first-syllable-stat)))
    ;; accurate regex for the first syllable and brute for other left part of string
    (if first-syllable-pos
        (let* (;; cut the first sullable
               (first-syllable (substring string 0 first-syllable-pos))
               ;; replace sub-syllable vowels with accurate regex
               (first-syllable (pinyin-isearch--make-syllable-to-regex
                                first-syllable first-syllable-stat))
               ;; if others is not null apply rough regex
               (others (if (< first-syllable-pos len)
                           ;; process others
                           (concat "\\s-*" (pinyin-isearch--brute-replace
                                            (substring st first-syllable-pos len)
                                            :normal t))
                         ;; else
                         nil)))
          (concat first-syllable others))
      ;; else - no syllable found
      (if (not pinyin-isearch-strict) st) ; if not strict search for original text
)))


;; ---------------------- part ---------------------


(defvar-local pinyin-isearch--original-search-default-mode search-default-mode)


(defun pinyin-isearch--fix-jumping-advice ()
  "Advice to fix isearch behavior.  Force search from a starting point."
  (if (and isearch-mode
           (eq isearch-regexp-function #'pinyin-isearch-regexp-function))
      (let ((key (this-single-command-keys)))
        ;; (print (lookup-key isearch-mode-map key nil) ) ; debug
        (when (and isearch-success
                   (eq 'isearch-printing-char (lookup-key isearch-mode-map key nil)))
          (goto-char isearch-opoint)
          (setq isearch-adjusted t)))))


;; (defadvice isearch-message-prefix (after pinyin-isearch-message-prefix activate)
;; "Add prefix when `search-default-mode' is used to make mode as default."
;;   (if (and ; (eq search-default-mode 'pinyin-isearch-regexp-function)
;;            (eq isearch-regexp-function 'pinyin-isearch-regexp-function))
;;       (setq ad-return-value (concat pinyin-isearch-message-prefix ad-return-value))
;;     ad-return-value))


(isearch-define-mode-toggle "pinyin-isearch" "n" pinyin-isearch-regexp-function "\
Turning on pinyin search turns off normal mode.")

(put 'pinyin-isearch-regexp-function 'isearch-message-prefix (format "%s " "[Pinyin]"))

(add-hook 'pre-command-hook 'pinyin-isearch--fix-jumping-advice)

;;;###autoload
(defun pinyin-isearch-forward (&optional regexp-p no-recursive-edit)
  "Do incremental search forward.
Optional argument REGEXP-P see original function `isearch-forward'.
Optional argument NO-RECURSIVE-EDIT see original function `isearch-forward'."
  (interactive "P\np")
  ;;save
  (setq pinyin-isearch--original-search-default-mode search-default-mode)
  (setq search-default-mode #'pinyin-isearch-regexp-function)
  (setq isearch-regexp-function #'pinyin-isearch-regexp-function)

  (if (called-interactively-p "any")
          (funcall-interactively #'isearch-forward regexp-p no-recursive-edit)
        ;; else
        (apply #'isearch-forward '(regexp-p no-recursive-edit)))
  ;; restore
  (setq search-default-mode pinyin-isearch--original-search-default-mode)
  )


;;;###autoload
(defun pinyin-isearch-backward (&optional regexp-p no-recursive-edit)
  "Do incremental search backward.
Optional argument REGEXP-P see original function `isearch-backward'.
Optional argument NO-RECURSIVE-EDIT see original function `isearch-backward'."
  (interactive "P\np")
  ;;save
  (setq pinyin-isearch--original-search-default-mode search-default-mode)
  (setq search-default-mode #'pinyin-isearch-regexp-function)
  (setq isearch-regexp-function #'pinyin-isearch-regexp-function)

  (if (called-interactively-p "any")
          (funcall-interactively #'isearch-backward regexp-p no-recursive-edit)
    ;; else
    (apply #'isearch-backward '(regexp-p no-recursive-edit)))
  ;; restore
  (setq search-default-mode pinyin-isearch--original-search-default-mode)
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
