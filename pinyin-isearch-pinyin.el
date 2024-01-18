;;; pinyin-isearch-pinyin.el --- Chinese pinyin search for isearch  -*- lexical-binding: t -*-

;; Copyright (c) 2024 Anoncheg1

;; Author: Anoncheg1
;; Keywords: chinese, pinyin, matching, convenience
;; URL: https://github.com/Anoncheg1/pinyin-isearch
;; Version: 1.6.4
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

;; How it works:
;; 1) we create list of ((\"zhuo\" . \"zhuō\")...) :
;;      pinyin-isearch-pinyin-syllable-table
;; 2) we define isearch-toggle-[pinyin] with
;;      isearch-define-mode-toggle macros
;; 3) we find first longest syllable and very accurate do regex
;;      with tones "n\\([ūúǔùǖǘǚǜ]e\\|ü[ēéěè]\\)" for the rest of
;;      the line we apply rough regex for every vowel [eēéěè]

;;; Code:

;; REQUIRE variable `pinyin-isearch-strict'

(require 'pinyin-isearch-loaders)

(declare-function string-replace "subr" (from-string to-string in-string)) ; to suppress warning

;; from package `pinyin-isearch-loaders'
;; (defvar sisheng-regexp :docstring "Located in quail/sisheng.")

;; (defvar sisheng-vowel-table :docstring "Located in quail/sisheng.")

;; (defvar sisheng-syllable-table :docstring "Located in quail/sisheng.")


(defconst pinyin-isearch-pinyin-vowel-table
  '(("a" "[āáǎà]")
    ("e" "[ēéěè]")
    ("i" "[īíǐì]")
    ("o" "[ōóǒò]")
    ("u" "[ūúǔùǖǘǚǜ]")
    ;; ("v" "[ūúǔùǖǘǚǜ]")
    ("ue" "ü[ēéěè]")
    ;; ("ve" "ü[ēéěè]")
))

(defconst pinyin-isearch-pinyin-vowel-table-normal
  '(("a" "[aāáǎà]")
    ("e" "[eēéěè]")
    ("i" "[iīíǐì]")
    ("o" "[oōóǒò]")
    ("u" "[uūúǔùǖǘǚǜ]")
    ("ue" "[uü][eēéěè]")
    ;; ("ve" "[uü][eēéěè]")
))

(defconst pinyin-isearch-pinyin-vowels
  '(("ā" "a")
    ("ē" "e")
    ("ī" "i")
    ("ō" "o")
    ("ū" "u")
    ("ǖ" "u")
    ("üē" "ue"))
    "Used to convert sisheng pinyin to toneless pinyin.")


(defun pinyin-isearch-pinyin--sisheng-to-normal (syllable)
  "Convert \"zhuō\" SYLLABLE to \"zhuo\".
Used to create list pinyin-isearch-pinyin-syllable-table from original
sisheng."
  (string-match sisheng-regexp syllable)
  (let* (;; fin vowel
        (vowel-match (downcase (match-string 0 syllable)))
        (vowel-list (cdr (assoc-string vowel-match sisheng-vowel-table)))
        (base-key (nth 0 vowel-list))
        (base-key (if (equal base-key "v") "u"
                     ;; else
                     (if (equal base-key "ve") "ue" base-key))))
    ;; fix for sisheng, we don't need "v"
    (replace-match base-key nil nil syllable)))


(defconst pinyin-isearch-pinyin-syllable-table
    (mapcar (lambda (arg)
              (cons (pinyin-isearch-pinyin--sisheng-to-normal arg) arg))
            sisheng-syllable-table) ;; sequence
    "Initialize syllable's table ((\"zhuo\" . \"zhuō\")...).")


(defun pinyin-isearch-pinyin--get_vowel_from_sisheng (string)
       "For input \"zuō\" get \"o\".
Uses: constant `pinyin-isearch-pinyin-vowels'.
Argument STRING sisheng syllable."
  (string-match sisheng-regexp string)
  (let* (
         (vowel-match (downcase (match-string 0 string)))
         (vowel-list
          (cdr (assoc-string vowel-match pinyin-isearch-pinyin-vowels))))
    (car vowel-list)))


(defun pinyin-isearch-pinyin--get-position-first-syllable(string)
  "Get position of the first syllable in query STRING.
It also return all vowels for all possible sub-syllables.
For \"zuom\" return (3 \"u\" \"o\").
Syllables with same tone vowel is ignored and used shortest.
Uses: function `pinyin-isearch-pinyin--get_vowel_from_sisheng'
and global variable `pinyin-isearch-pinyin-syllable-table'."
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
      (setq syl (cdr (assoc first-chars pinyin-isearch-pinyin-syllable-table)))
      ;; if syllable found, add vowel with tone to vowels
      (if syl
          (let ((new-vow (pinyin-isearch-pinyin--get_vowel_from_sisheng syl)))
            (if (not (member new-vow vowels))
                (setq vowels (cons (pinyin-isearch-pinyin--get_vowel_from_sisheng syl) vowels))
              ;; else ignore previous long syllable with same vowel
              (setq ret nil))))
      ;; save position of the first longest syllable
      (if (and (null ret) syl)
          (setq ret pos))
      (setq pos (1- pos)))
    ;; -- let:
    (cons ret vowels)))


(defun pinyin-isearch-pinyin--vowels-to-regex (vowels)
  "Used for accurate apply regex to the first syllable of toneless pinyin.
Convert (u o) to
\"\\([ūúǔùǖǘǚǜ][oōóǒò]\\|[uūúǔùǖǘǚǜ][ōóǒò]\\)\"
and (u) to \"[ūúǔùǖǘǚǜ]\".  Uses tables:
`pinyin-isearch-pinyin-vowel-table',
`pinyin-isearch-pinyin-vowel-table-normal'.  Argument VOWELS list of
normal vowels."
  (if (eq (length vowels) 2)
      (if  (not (member "ue" vowels))
          (let* ((pin-vowels (mapcar (lambda (x)
                                       (assoc-string x pinyin-isearch-pinyin-vowel-table)) vowels))
                 (pin-vowels1 (car (cdr (car pin-vowels))))
                 (pin-vowels2 (car (cdr (car (cdr pin-vowels))))))
            (concat "\\(" pin-vowels1 "\\s-*" (car (cdr vowels))
                    "\\|" (car vowels) pin-vowels2 "\\)"))
        ;; ("u" "ue") case
        ;; (print (cdr (assoc-string "ue" pinyin-isearch-pinyin-vowel-table)))
        (let ((p1  (concat (car (cdr (assoc-string "u" pinyin-isearch-pinyin-vowel-table))) "\\s-*e")) ; [ūúǔùǖǘǚǜ]e
              (p2 (car (cdr (assoc-string "ue" pinyin-isearch-pinyin-vowel-table))))  ; "ü[ēéěè]"
              )
          (concat "\\(" p1 "\\|" p2 "\\)") ; "\\([ūúǔùǖǘǚǜ]e\\|ü[ēéěè]\\)"
          ))
    ;; else if one vowel
    (car (cdr (assoc-string (car vowels) pinyin-isearch-pinyin-vowel-table)))))


(defun pinyin-isearch-pinyin--make-syllable-to-regex (syllable d-vowels)
  "Convert SYLLABLE \"zhuo\" to \"zh([]\|[])\".
Applyed to the first syllable to create accurate regex.
Uses function `pinyin-isearch-pinyin--vowels-to-regex'.
Argument D-VOWELS result of function
 `pinyin-isearch-pinyin--get-position-first-syllable'."
  (if (not (car d-vowels))
      syllable
    ;; else
    (let ((vowels-conc (if (member "ue" d-vowels) "ue"
                         ;; else
                         (apply #'concat (cdr d-vowels)))) ; "uo"
          (replacement (pinyin-isearch-pinyin--vowels-to-regex (cdr d-vowels))))
      (string-replace vowels-conc replacement syllable))))


(defun pinyin-isearch-pinyin--brute-replace (st &optional &key normal)
  "Replace every vowels in ST with wide range regex.
if NORMAL add normal to regex."
  (let* (
         ;; ignore white spaces if query is more than 2 characters
         (st (if (> (length st) 1)
                 ;; then - insert whitespaces between every character
                 (concat (substring st 0 1)
                         (mapconcat (lambda (x) (concat "\\s-*" x))
                                    (cdr (split-string st "" t))
                                    nil))
               ;; else
               st)))
    ;; ignore tones, based on lisp/leim/quail/sisheng.el
    (if normal
        (dolist ( c (split-string "aeiou" "" t))
          (let ((vowel-list-regex
                 (car (cdr (assoc-string c pinyin-isearch-pinyin-vowel-table-normal))) ))
            (setq st (string-replace c vowel-list-regex st))))
      ;; else (not used now)
      (dolist ( c (split-string "aeiou" "" t))
          (let ((vowel-list-regex
                 (car (cdr (assoc-string c pinyin-isearch-pinyin-vowel-table))) ))
            (setq st (string-replace c vowel-list-regex st)))))
    st))


(defun pinyin-isearch-pinyin-regexp-f (string)
  "Replacement for function `isearch-regexp-function'.
Main function to convert query STRING to regex for isearch.
Uses functions: `pinyin-isearch-pinyin--get-position-first-syllable',
`pinyin-isearch-pinyin--make-syllable-to-regex',
`pinyin-isearch-pinyin--brute-replace'."

  (let* ((st (regexp-quote string))
         ;; save length
         (len (length st))
         ;; get the first longest syllable
         (first-syllable-stat (if (> (length st) 1)
                                  (pinyin-isearch-pinyin--get-position-first-syllable st)
                                ;; else
                                '(nil)))
         (first-syllable-pos (car first-syllable-stat)))
    ;; accurate regex for the first syllable and brute for other left part of string
    (if first-syllable-pos
        (let* (;; cut the first sullable
               (first-syllable (substring string 0 first-syllable-pos))
               ;; replace sub-syllable vowels with accurate regex
               (first-syllable (pinyin-isearch-pinyin--make-syllable-to-regex
                                first-syllable first-syllable-stat))
               ;; if others is not null apply rough regex
               (others (if (< first-syllable-pos len)
                           ;; process others
                           (concat "\\s-*" (pinyin-isearch-pinyin--brute-replace
                                            (substring st first-syllable-pos len)
                                            :normal t))
                         ;; else
                         nil)))
          (concat first-syllable others))
      ;; else - no syllable found
      (if (not pinyin-isearch-strict) st) ; if not strict search for original text
)))

(defvar-local pinyin-isearch-pinyin--saved-query nil
  "For `pinyin-isearch-pinyin-regexp-function'.")
(defvar-local pinyin-isearch-pinyin--saved-regex nil
  "For `pinyin-isearch-pinyin-regexp-function'.")
(defvar-local pinyin-isearch-pinyin--saved-strict nil
  "For `pinyin-isearch-pinyin-regexp-function'.")

(defun pinyin-isearch-pinyin-regexp-function (string &optional lax)
  "Replacement for function `isearch-regexp-function'.
Optional argument LAX not used.
Argument STRING is query."
  (setq lax lax) ; suppers Warning: Unused lexical argument `lax'

  ;; check that pinyin-isearch-strict did not changed
  (when (not (eq pinyin-isearch-pinyin--saved-strict pinyin-isearch-strict))
    (setq pinyin-isearch-pinyin--saved-query nil)
    (setq pinyin-isearch-pinyin--saved-regex nil)
    (setq pinyin-isearch-pinyin--saved-strict pinyin-isearch-strict))
  ;; this check required for speed optimization for isearch repeated calls
  (if (equal string pinyin-isearch-pinyin--saved-query)
      pinyin-isearch-pinyin--saved-regex
    ;; else
    (progn
      (setq pinyin-isearch-pinyin--saved-query string)
      (setq pinyin-isearch-pinyin--saved-regex
            (pinyin-isearch-pinyin-regexp-f string)))))

(provide 'pinyin-isearch-pinyin)
;;; pinyin-isearch-pinyin.el ends here
