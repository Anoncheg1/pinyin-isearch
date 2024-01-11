;;; pinyin-isearch-hieroglyphs.el --- isearch mode for Chinese hieroglyphs search.  -*- lexical-binding: t -*-

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

;; Allow to search with Chinese pinyin in pinyin text and ignore
;; diacritical tone marks for speed.
;; Features:
;; - white spaces are ignored between syllables,
;;   but not ignored if in query
;; - tone required in text only for first syllable: Zhēn de ma
;; - should not conflict with other isearch modes
;; - search do not jump down but always begins from start point.

;; How it works:
;; 1) Split toneless pinyin to variants of syllables, final syllables
;; may be unfinished.
;; 2) Filter variants with full syllables if there is all
;; kidnds of variants.
;; 3) Convert toneless pinyin syllables to hieroglyphs.
;; 4) Make accurate regex from result list of lists with hieroglyphs.
;; Solved problems: How to store charaters that is not part
;; of any syllable? Special marker before string \34 was used
;; to make it fast.
;; What to do with nv and lv and lu and lu pinyin?
;; We added lv and nv to lu and nu, but leave lv and nv as they was.
;; What to do with characters that is used regex?
;; We escaped all strings in function
;; `pinyin-isearch--pinyin-to-hieroglyphs'.

;;; Code:

(require 'pinyin-isearch-loaders)

(defgroup pinyin-isearch nil
  "Fuzzy matching."
  :group 'pinyin-isearch
  :prefix "pinyin-isearch-")

(defcustom pinyin-isearch-strict nil
  "Non-nil means enforce to search only hierogliphs.
isearch will not fallback to find normal latin text if pinyin is
not found."
  :local t
  :type 'boolean
  :group 'pinyin-isearch)


;; ---------- prepare syllable table ---------
(defun pinyin-isearch--rules-to-first-syllable-letters (rules)
  "Create table that allow quickly find syllable by it's first letters.
Argument RULES argument of funcion `quail-define-rules'."
  (let ((ss nil))
    (dolist ( r rules)
      (let ((rr (car r)))
        (dolist (l (number-sequence 1 (length rr)))
          (let* ((sub (substring (car r) 0 l))
                 (el (assoc-string sub ss))
                 (newl))
            (when (not el) ;; el is nil
              (dolist ( r rules)
                (if (string-prefix-p sub (car r))
                    (push (car r) newl)))
              (setq ss (cons (list sub newl) ss))
              )
            )
          )))
    ss))


(defconst pinyin-isearch-first-syllable-letters
  (pinyin-isearch--rules-to-first-syllable-letters pinyin-isearch--py-punct-rules)
"This table allow to quickly find all syllables by their first letters.
\((a (ao ang an ai a)) ...)")

(defconst pinyin-isearch--non-syllable-marker-number 28
  "Used to distringuish non syllable sequences from syllables after split.") ;\34

(defconst pinyin-isearch--non-syllable-marker-string "\34")

;; ----------- tools -----------
(defun pinyin-isearch--get-syllables-by-prefix (st)
  "Interface to constant `pinyin-isearch-first-syllable-letters'.
For \"a\" we get (ao ang an ai a).
Argument ST the begining letters of any syllable."
  (let ((v (assoc-string st pinyin-isearch-first-syllable-letters )))
    (if v
        (let (( res (copy-tree (car (cdr v)))))
          ;; remove nv and lv from result
          (cond ((equal st "n") (setq res (remove "nv" res)))
                ((equal st "l") (setq res (remove "lv" res))))
          res)
    ;; else
    nil)))


(defun pinyin-isearch--pinyin-to-hieroglyphs (syl)
  "Interface to constant `pinyin-isearch--py-punct-rules'.
For syllable \"an\" we get \"昂肮盎\".
Argument SYL syllable of toneless pinyin."
  ;; this if for speed optimization only
  (if (eq (elt syl 0) pinyin-isearch--non-syllable-marker-number)
      (regexp-quote syl)
    ;; else
    (let ((r (assoc-string syl pinyin-isearch--py-punct-rules))) ; TODO: make rules as a variable
      (if r
          (if pinyin-isearch-strict
              (car (cdr r))
            ;; else, fix for "."
            (regexp-quote (string-replace "．" "．." (car (cdr r)))))
        ;;else
        (regexp-quote syl)
      ))))


(defun pinyin-isearch--hieroglyphs-recursion (st)
  "Split string to variants of splits to pinyin syllables.
Return variants of separateion (variant1 variant2), where
variant1 is a list of variants of hieroglyphs

\((hv1 hv2 hv3) (hv1 hv3...) ...)  what inside: 1) variants of
disassembly 2) hieroglyphs 3) variants of hieroglyphs.
Variants of hieroglyphs used for final syllable when we try to guess
that hieroglyphs begining we have.

Steps:
1. in loop find syllables in 0-6 first letters.
2. recursive call for right (left) part
In 1. if it is last letters than we use hungry search
3. add syllable to every variant of right part at level 2)
4. concat all found variants of dissasembly at level 3)
Argument ST user input string for isearch search."
  (let* ((len_max (length st)) ; all len
        (len (if  (<= len_max 6) len_max 6)) ; 0-6 len
        (pos 1)
        (first-chars) ; per loop
        (syllables) ; per loop
        (finals)) ; accamulate found variants of disassembly by first found syllable
    (while (<= pos len)
      (setq first-chars (substring st 0 pos))
      ;; - - find syllables for the first part - -
      (if (eq pos len_max) ; last while
          ;; if last letters we find uncompleted syllables
          (setq syllables (pinyin-isearch--get-syllables-by-prefix first-chars))
        ;; else if it is not last symbols we find only full one syllable
        (progn
          (setq syllables (copy-sequence (assoc-string first-chars pinyin-isearch--py-punct-rules))) ; copy to prevent destruction. TODO: make as variable
          (if syllables (setq syllables (list (car syllables))) )))

      (when syllables ; variants of one hierogliph
        (let ((fin)) ; current part of finals - ((( )))
          ;; recurse call for left letters:
          (if (> (- len_max pos) 0)
              ;; when there is left characters - we do recursive call. Syllables is one.
              (let* ((left-let (substring st pos len_max))
                     (left-rec (pinyin-isearch--hieroglyphs-recursion left-let))) ; ((( ))) - result of recursion
                (if left-rec
                    (setq fin (mapcar (lambda (x) (cons syllables x)) left-rec))
                  ;; else
                  ;; (setq fin (list (list syllables))))
                ))
            ;; else - add only syllable as a single hieroglyph - no left was. Syllables is many
            (setq fin (list (list syllables) ))
            ) ; end of if
          (setq finals (cons fin finals))
          )) ; end of when and let

      (setq pos (1+ pos)) ; pos+=1
      ) ; end of while
    (if (null finals)
        ;; 1) variants of disassembly 2) variant 3) hieroglyph
        ;; we add marker to tag that it is not a syllable
        (if pinyin-isearch-strict
            nil
          ;; else
          (list (list (list (concat pinyin-isearch--non-syllable-marker-string st)) )))
      ;; else
      (setq finals (nreverse finals)) ; reverse
      (apply 'append finals)) ; flatten by one level
    )) ; end of let*



(defun pinyin-isearch--filter-full-variants (f l)
  "Filter variants that has unfinished letters at the end.
Variants of disassemble.  If there is only variants with
unfinished letters, we don't filter them.
Function `F' convert pinyin to hierogliph.
Steps: 1) filter variants ending with hieroglyphs
2) return filtered varians or all if filtered is nil.
Argument L is a list of disassemble variants."
  (or (seq-filter 'identity (mapcar (lambda (x)
            (let ((last (car (nth (1- (length x)) x))))
              (if (not (equal (funcall f last) last))
                  x))
            ) l)) l)
  )


(defun pinyin-isearch--maptree (f l)
  "Apply map to every leaf of a list.
Argument F function that will be applyed to leafs.
Argument L list with any structure of sublists."
  (mapcar (lambda (x) (if (listp x)
                          (pinyin-isearch--maptree f x)
                        ;; else
                        (funcall f x)
                        )) l))

(defun pinyin-isearch--convert-to-hieroglyphs (list-of-variants)
  "For every leaf of splitted request apply converter to hieroglyphs.
Argument LIST-OF-VARIANTS list that is result of function
`pinyin-isearch--hieroglyphs-recursion'."
  (pinyin-isearch--maptree 'pinyin-isearch--pinyin-to-hieroglyphs list-of-variants)
)

(defun pinyin-isearch--regex-concat-hieroglyphs (l)
  "昂肮盎 to [昂肮盎] and concat such strings.
This is done for every variant of syllable.
Argument L list of form ((\"gg\"))."
  (mapconcat (lambda (x)
               ;; apply to every ("sd" "sd") or ("sd")
               (let ((cx (car x)))
                 (if (or (eq (length x) 1))
                     (if  ; first character equel ""
                         (eq (elt cx 0) pinyin-isearch--non-syllable-marker-number)
                         (substring cx 1) ; delete first character
                       ;; else
                       (concat "[" cx "]")
                       )
                   ;; else ("sd" "sd")
                   (concat "[" (apply 'concat x) "]"))))
             l nil))

(defun pinyin-isearch--concat-variants (sac)
"Create regex alternation for dissasemble variants.
Argument SAC is splitted-and-converted variants."
  (if (> (length sac) 1)
      (concat "\\(" (mapconcat 'pinyin-isearch--regex-concat-hieroglyphs
                               sac "\\|") "\\)")

    ;; else eq 1
    (pinyin-isearch--regex-concat-hieroglyphs (car sac)))
)


(defun pinyin-isearch-hieroglyphs-regexp-function (string &optional lax)
  "Replacement for function `isearch-regexp-function'.
How it works, in step:
 1) split to parts according to pinyin.
2) filter variants that do not endings without pinyin.
3) convert every syllable to hierogliphs.
4) surround variants of syllables with [], concat hieroglyphs and
concat variants with \\|.
Argument STRING isearch user input string of query.
Optional argument LAX used for isearch special cases."
  (setq lax lax) ; suppers Warning: Unused lexical argument `lax'

  (let* (;;(st (regexp-quote string))
        (splitted-and-converted (pinyin-isearch--convert-to-hieroglyphs
                                 (pinyin-isearch--filter-full-variants
                                  'pinyin-isearch--pinyin-to-hieroglyphs
                                  (pinyin-isearch--hieroglyphs-recursion string)))))
    (pinyin-isearch--concat-variants splitted-and-converted)
    ))



(provide 'pinyin-isearch-hieroglyphs)
;;; pinyin-isearch-hieroglyphs.el ends here
