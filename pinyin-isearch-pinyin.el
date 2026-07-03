;;; pinyin-isearch-pinyin.el --- Chinese pinyin search for isearch  -*- lexical-binding: t -*-

;; Copyright (c) 2024 Anoncheg1

;; Author: Anoncheg1
;; Keywords: chinese, pinyin, matching, convenience
;; URL: https://github.com/Anoncheg1/pinyin-isearch
;; Version: 1.6.9
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


;; Allow to search with Chinese pinyin diacritical tone marks for
;;  pinyin with diacritical tone marks with or without abolity to
;;  fallback to normal latin text.

;; Features:
;; - white spaces are ignored between syllables,
;;   but not ignored if in query
;; - Tone required in text only for first syllable: Zhēn de ma
;; - should not conflict with other isearch modes
;; - search do not jump down but always begins from start point.

;;;; How it works:

;; 1) we create list of ((\"zhuo\" . \"zhuō\")...) :
;;      `pinyin-isearch-pinyin-syllable-table'
;; 2) we define isearch-toggle-[pinyin] with
;;      isearch-define-mode-toggle macros
;; 3) we find first longest syllable and very accurate do regex
;;      with tones "n\\([ūúǔùǖǘǚǜ]e\\|ü[ēéěè]\\)" for the rest of
;;      the line we apply rough regex for every vowel [eēéěè]


;; 1) create own table with `pinyin-isearch-pinyin--sisheng-to-normal'
;; and `sisheng-vowel-table', form: (orig . our + patch v->u, ve->ue.)
;;
;; 2) For input: `pinyin-isearch-pinyin--get-position-first-syllable'
;; which uses our syllable-table to find \"zhuō\" and
;; `pinyin-isearch-pinyin--get_vowel_from_sisheng' which uses
;; `pinyin-isearch-pinyin-vowels' to get "ue" from "üē".

;;; Code:

;; REQUIRE variable `pinyin-isearch-strict'

(require 'pinyin-isearch-loaders)

(require 'cl-lib)

(declare-function pinyin-isearch-loaders-load-chinese-sisheng "pinyin-isearch-loaders") ; load sisheng variables

(defvar pinyin-isearch-strict) ; "Located in `pinyin-isearch'."

(defvar pinyin-isearch-full-fallback) ; "Located in `pinyin-isearch.el'."
;; used in `pinyin-isearch-pinyin--vowels-to-regex' for vowels

;; from package `pinyin-isearch-loaders'
(defvar sisheng-regexp) ; "Located in quail/sisheng."

(defvar sisheng-vowel-table) ; "Located in quail/sisheng."

(defvar sisheng-syllable-table) ; "Located in quail/sisheng."


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
  "Fix tones in syllable with `sisheng-regexp' and `sisheng-vowel-table'.
Used to create list `pinyin-isearch-pinyin-syllable-table' from original
 sisheng.
Given a syllable like \"zhuō\" (with tone marks) returns \"zhuo\",
 suitable for pinyin input searches.  Handles special cases where 'ü' or
 'ü' with tone marks appear (such as \"lüè\"), converting those to
 \"lue\".  If no tone marked vowel is detected, returns the input
 unchanged.
SYLLABLE: String representing a pinyin syllable, possibly with tone
 marks.
Requires `sisheng-regexp' and `sisheng-vowel-table' to be defined."
  ;; Try to match tone-marked vowel in the syllable
  (if (string-match sisheng-regexp syllable)
      (let* ((matched-vowel (downcase (match-string 0 syllable)))
             ;; Lookup normalized vowel in table
             (vowel-list (cdr (assoc-string matched-vowel sisheng-vowel-table)))
             (base-vowel (when vowel-list (nth 0 vowel-list))))
        ;; Special handling for 'v' and 've' which represent 'ü' and 'üe'
        (cond
         ((equal base-vowel "v") (setq base-vowel "u"))
         ((equal base-vowel "ve") (setq base-vowel "ue")))
        ;; Replace matched vowel with normalized vowel, return new syllable
        (replace-match base-vowel nil nil syllable))
    ;; If no tone-marked vowel, return input unchanged
    syllable))


(defvar pinyin-isearch-pinyin-syllable-table nil
  "Initialize syllable's table ((\"zhuo\" . \"zhuō\")...).")

(defun pinyin-isearch-pinyin-load ()
  "Initialize variable `pinyin-isearch-pinyin-syllable-table'."
  (unless pinyin-isearch-pinyin-syllable-table
    (pinyin-isearch-loaders-load-chinese-sisheng)
    (setq pinyin-isearch-pinyin-syllable-table
          (mapcar (lambda (arg)
                    (cons (pinyin-isearch-pinyin--sisheng-to-normal arg) arg))
                  sisheng-syllable-table))))


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


(defun pinyin-isearch-pinyin--get-position-first-syllable (string)
  "Get position of the first syllable in query STRING.
It also return all vowels for all possible sub-syllables.
For \"zuom\" return (3 \"u\" \"o\").
Syllables with same tone vowel is ignored and used shortest.
Uses: function `pinyin-isearch-pinyin--get_vowel_from_sisheng'
and global variable `pinyin-isearch-pinyin-syllable-table'."
  (let ((first-chars)
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
`pinyin-isearch-pinyin-vowel-table-normal'.
If `pinyin-isearch-full-fallback' variable is true use
 `pinyin-isearch-pinyin-vowel-table-normal' to match pinyin without
 tones.
Argument VOWELS list of normal vowels."
  (let ((vowel-table (if (and (not pinyin-isearch-strict)
                              pinyin-isearch-full-fallback)
                         pinyin-isearch-pinyin-vowel-table-normal
                       ;; else
                       pinyin-isearch-pinyin-vowel-table)))
    (if (eq (length vowels) 2)
        (if  (not (member "ue" vowels))
            (let* ((pin-vowels (mapcar (lambda (x)
                                         (assoc-string x vowel-table)) vowels))
                   (pin-vowels1 (car (cdr (car pin-vowels))))
                   (pin-vowels2 (car (cdr (car (cdr pin-vowels))))))
              (concat "\\(" pin-vowels1 "\\s-*" (car (cdr vowels))
                      "\\|" (car vowels) pin-vowels2 "\\)"))
          ;; ("u" "ue") case
          ;; (print (cdr (assoc-string "ue" vowel-table)))
          (let ((p1  (concat (car (cdr (assoc-string "u" vowel-table))) "\\s-*e")) ; [ūúǔùǖǘǚǜ]e
                (p2 (car (cdr (assoc-string "ue" vowel-table))))  ; "ü[ēéěè]"
                )
            (concat "\\(" p1 "\\|" p2 "\\)") ; "\\([ūúǔùǖǘǚǜ]e\\|ü[ēéěè]\\)"
            ))
      ;; else if one vowel
      (car (cdr (assoc-string (car vowels) vowel-table))))))



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


(defun pinyin-isearch-pinyin--brute-replace (st &optional normal)
  "Expand vowels in ST into regex form, separating by \\s-*.
If NORMAL is non-nil, include the original vowel."
  (let* ((vowel-table (if normal pinyin-isearch-pinyin-vowel-table-normal
                        pinyin-isearch-pinyin-vowel-table))
         (result ()))
    (dolist (c (split-string st "" t))
      (when result (push "\\s-*" result))
      (push (or (cadr (assoc c vowel-table)) c) result))
    (apply #'concat (nreverse result))))

;; (pinyin-isearch-pinyin--brute-replace "hao") ;; => "h\\s-*[āáǎà]\\s-*[ōóǒò]"
;; (pinyin-isearch-pinyin--brute-replace "haoh" t) ;; => "h\\s-*[aāáǎà]\\s-*[oōóǒò]\\s-*h"


(defun pinyin-isearch-pinyin-regexp-sub (string)
  "Convert query STRING to regex for isearch, preserving original logic.
Called from `pinyin-isearch-pinyin-regexp-function'.
Uses functions:
- `pinyin-isearch-pinyin--get-position-first-syllable',
-`pinyin-isearch-pinyin--make-syllable-to-regex',
-`pinyin-isearch-pinyin--brute-replace'."
  (let* ((st (regexp-quote string))
         (len (length st))
         (first-syllable-stat (if (> len 1)
                                  (pinyin-isearch-pinyin--get-position-first-syllable st)
                                '(nil)))
         (first-syllable-pos (car first-syllable-stat)))
    (if first-syllable-pos
        (let* ((first-syllable (substring string 0 first-syllable-pos))
               (first-syllable (pinyin-isearch-pinyin--make-syllable-to-regex
                                first-syllable first-syllable-stat))
               (others (when (< first-syllable-pos len)
                         (concat "\\s-*"
                                 (pinyin-isearch-pinyin--brute-replace (substring st first-syllable-pos) t)))))
          (concat first-syllable others))
      ;; else - no first-syllable
      (if (or (not pinyin-isearch-full-fallback)
              pinyin-isearch-strict
              (string-empty-p string))
          "$^"
        ;; else
        st))))

;; (pinyin-isearch-pinyin-regexp-sub "hao")
;; ;; => "h\\s-*[āáǎà]\\s-*[ōóǒò]"

;; (pinyin-isearch-pinyin-regexp-sub "haoh")
;; ;; => "h\\s-*[aāáǎà]\\s-*[oōóǒò]\\s-*h"

;; (pinyin-isearch-pinyin-regexp-sub "zuo")
;; ;; => "z\\([uūúǔùǖǘǚǜ]\\s-*o\\|u[oōóǒò]\\)"

(defvar-local pinyin-isearch-pinyin--cached-query nil
  "For `pinyin-isearch-pinyin-regexp-function'.")
(defvar-local pinyin-isearch-pinyin--cached-regex nil
  "For `pinyin-isearch-pinyin-regexp-function'.")
(defvar-local pinyin-isearch-pinyin--cached-strict nil
  "For `pinyin-isearch-pinyin-regexp-function'.")
(defvar-local pinyin-isearch-pinyin--cached-full-fallback nil
  "For `pinyin-isearch-pinyin-regexp-function'.")

(defun pinyin-isearch-pinyin-regexp-function (string &optional _lax) ;TODO: make it support pinyin-isearch-full-fallback
  "Replacement for function `isearch-regexp-function'.
Optional argument LAX not used.
Argument STRING is query."
  (unless pinyin-isearch-pinyin-syllable-table
    (user-error "(pinyin-isearch-pinyin-load) was not called"))

  ;; check if match cached one
  (when (or (not (eq pinyin-isearch-pinyin--cached-strict pinyin-isearch-strict))
            (not (eq pinyin-isearch-pinyin--cached-full-fallback pinyin-isearch-full-fallback))
            (not (string-equal string pinyin-isearch-pinyin--cached-query)))

    (setq pinyin-isearch-pinyin--cached-query string)
    (setq pinyin-isearch-pinyin--cached-regex
          (pinyin-isearch-pinyin-regexp-sub string))) ; MAIN call

  pinyin-isearch-pinyin--cached-regex)

(provide 'pinyin-isearch-pinyin)
;;; pinyin-isearch-pinyin.el ends here
