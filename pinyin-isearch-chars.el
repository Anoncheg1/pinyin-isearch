;;; pinyin-isearch-chars.el --- Chinese characters search for isearch -*- lexical-binding: t -*-

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

;; Fuzzy matching by pinyin. Allow to search Chinese characters by
;;  typing pinyin without diacritical tone marks,
;;  may fallback to normal latin text.

;; Features:
;; - white spaces are ignored between syllables, but not ignored if in
;;  query
;; - should not conflict with other isearch modes
;; - search do not jump down but always begins from start point.

;;;; How it works:
;; 1) Split toneless pinyin to variants of syllables, final syllables
;;  may be unfinished.  `pinyin-isearch-chars--recursion'
;; 2) Filter variants with full syllables if there is all
;;  kidnds of variants.
;; 3) Convert toneless pinyin syllables to hieroglyphs.
;; 4) Make accurate regex from result list of lists with hieroglyphs.
;;  Solved problems: How to store charaters that is not part
;;  of any syllable? Special marker before string \34 was used
;;  to make it fast.
;; What to do with nv and lv and lu and lu pinyin?
;; We added lv and nv to lu and nu, but leave lv and nv as they was.
;; What to do with characters that is used regex?
;; We escaped all strings in function
;;  `pinyin-isearch--pinyin-to-hieroglyphs'.

;;; Code:

(require 'pinyin-isearch-loaders) ; for pinyin-isearch-loaders--py-punct-rules

(require 'cl-extra)

(defgroup pinyin-isearch nil
  "Pinyin-isearch customization."
  :group 'pinyin-isearch)

(defvar pinyin-isearch-strict) ; (require 'pinyin-isearch)

(defvar pinyin-isearch-full-fallback) : ; (require 'pinyin-isearch)

(defcustom pinyin-isearch-chars-fallback t
  "Non-nil means add pinyin that was not matched.
full query string as a regex variant.
But only if pinyin cant be interpreted as any characters (before even
 search).
If there is undecoded letters at the end after dissasembling."
  :local t
  :type 'boolean
  :group 'pinyin-isearch)


;; ---------- loaded punct and concatenate: py + punct --------
(defvar pinyin-isearch-chars--py-rules nil
  "Rules in form: ((\"a\" \"阿啊呵腌嗄锕吖\") (\"ai\" \"爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹\")...")

(defvar pinyin-isearch-chars--punct-rules nil
  "Extracted and filtered Chinese punctuation.")

(defvar pinyin-isearch-chars--py-punct-rules nil
  "Extracted quail/PY.el + quail/Punct.el - Chinese heieroglyphs and punctuation.")

(defvar pinyin-isearch-chars--first-syllable-letters nil
"This table allow to quickly find all syllables by their first letters.
\((a (ao ang an ai a)) ...)")


(defconst pinyin-isearch-chars--non-syllable-marker-number 28
  "Used to distringuish non syllable sequences from syllables after split.") ;\34

(defconst pinyin-isearch-chars--non-syllable-marker-string "\34") ; or "\x1c"


;; ---------- prepare syllable table ---------
(defun pinyin-isearch-chars--rules-to-first-syllable-letters (rules)
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
              (setq ss (cons (list sub newl) ss)))))))
    ss))

(defun pinyin-isearch-chars-load ()
  "Prepare variables from `pinyin-isearch-loaders'."
  (unless pinyin-isearch-chars--first-syllable-letters
    (setq pinyin-isearch-chars--py-rules (pinyin-isearch-loaders--py-rules-loader))
    (setq pinyin-isearch-chars--punct-rules
          (pinyin-isearch-loaders--punct-quail-filter
           (pinyin-isearch-loaders--quail-extractor "chinese-punct")))
    (setq pinyin-isearch-chars--py-punct-rules
          (append pinyin-isearch-chars--py-rules pinyin-isearch-chars--punct-rules))
    (setq pinyin-isearch-chars--first-syllable-letters
          (pinyin-isearch-chars--rules-to-first-syllable-letters pinyin-isearch-chars--py-punct-rules))))

;; ----------- tools -----------
(defun pinyin-isearch-chars--get-syllables-by-prefix (st)
  "Interface to constant `pinyin-isearch-chars--first-syllable-letters'.
For \"a\" we get (ao ang an ai a).
Argument ST the begining letters of any syllable."
  (let ((v (assoc-string st pinyin-isearch-chars--first-syllable-letters )))
    (if v
        (let (( res (copy-tree (car (cdr v)))))
          ;; remove nv and lv from result
          (cond ((equal st "n") (setq res (remove "nv" res)))
                ((equal st "l") (setq res (remove "lv" res))))
          res)
    ;; else
    nil)))


(defun pinyin-isearch-chars--pinyin-to-hieroglyphs (syl)
  "Interface to constant `pinyin-isearch-chars--py-punct-rules'.
For syllable \"an\" we get \"昂肮盎\".
Argument SYL syllable of toneless pinyin."
  ;; this if for speed optimization only
  (if (eq (elt syl 0) pinyin-isearch-chars--non-syllable-marker-number)
      (regexp-quote syl)
    ;; else
    (let ((r (assoc-string syl pinyin-isearch-chars--py-punct-rules))) ; TODO: make rules as a variable
      (if r
          (if pinyin-isearch-strict
              (car (cdr r))
            ;; else, fix for "." we add normal dot
            (regexp-quote (string-replace "．" "．." (car (cdr r)))))
        ;;else
        (regexp-quote syl)))))


;; Old docstring:
;;   "Split string to variants of splits to pinyin syllables.
;; Return variants of separateion (variant1 variant2), where
;; variant1 is a list of variants of hieroglyphs

;; \((hv1 hv2 hv3) (hv1 hv3...) ...)  what inside: 1) variants of
;; disassembly 2) hieroglyphs 3) variants of hieroglyphs.
;; Variants of hieroglyphs used for final syllable when we try to guess
;; that hieroglyphs begining we have.


(defun recursion-core (st)
  "Split string ST recursively into all valid pinyin syllable combinations.
For each prefix (up to 6 characters), checks if it is a valid pinyin
 syllable or punctuation.
If valid, recursively splits the remainder of ST.  Adds fallback variants
 if enabled.
Returns a list of lists, each representing one possible syllable split
 or fallback variant."
  (let ((len (length st))
        (maxlen (min 6 (length st)))
        results)
    (when (> len 0)
      (dotimes (i maxlen)
        (let* ((end (1+ i))
               (prefix (substring st 0 end))
               (sylls (if (or pinyin-isearch-strict (< end len))
                          (and (assoc-string prefix pinyin-isearch-chars--py-punct-rules)
                               (list prefix))
                        (pinyin-isearch-chars--get-syllables-by-prefix prefix))))
          (when sylls
            (let ((rest (substring st end)))
              (cond
               ((zerop (length rest))
                (push (list sylls) results))
               (t
                (let ((rest-variants (recursion-core rest)))
                  (when pinyin-isearch-chars-fallback
                    (push (list sylls (list (concat pinyin-isearch-chars--non-syllable-marker-string rest))) results))
                  (dolist (variant rest-variants)
                    (push (cons sylls variant) results))))))))))
    results))

(defun pinyin-isearch-chars--recursion (st)
  "Return all valid pinyin splits (and fallback variants) for string ST.
Recursively splits ST into syllable lists using `recursion-core`.  If no
 valid split and fallback is enabled, returns fallback variant prefixed
 with marker.
Uses variables:
  - `pinyin-isearch-strict'
  - `pinyin-isearch-chars--py-punct-rules'
  - `pinyin-isearch-chars--get-syllables-by-prefix'
  - `pinyin-isearch-chars--non-syllable-marker-string'
Full fallback added only if no syllable was found with
 `pinyin-isearch-chars--get-syllables-by-prefix' function or in
 `pinyin-isearch-chars--py-punct-rules' table.
if no syllable found but fallback is t
Returns a list of list of syllable lists or
 nil if ST cannot be split and fallback is disabled."
  (let ((results (recursion-core st)))
    (cond
     ((and (not results)
           (not pinyin-isearch-strict)
           pinyin-isearch-chars-fallback
           (> (length st) 0))
      (list (list (list (concat pinyin-isearch-chars--non-syllable-marker-string st)))))
     (t results))))



(defun pinyin-isearch-chars--add-fallback (string lvar)
  "Add full string to desiassembled variants.
If at the end of query there is unconvertable letters.  Global
variable `pinyin-isearch-strict' is used here.  Argument STRING
original request to add for fallback when strict mode is not
activated.
Argument LVAR dissasembled variants of characters for query."
  ;; add full string for fallback to latin if we have unconvertable characters at the end (marked)
  (if (and pinyin-isearch-chars-fallback
           (not pinyin-isearch-strict)
           (or (> (length lvar) 1) (> (length (car lvar)) 1)))
      (let ((la (car (car (last (car (last lvar)))))))
        (if (eq (elt la 0) pinyin-isearch-chars--non-syllable-marker-number)
            ;; add full string (marked) to result list as an another variant.
            (setq lvar (cons (list (list (concat pinyin-isearch-chars--non-syllable-marker-string string))) lvar)))))
  lvar)

;; replaced by logic in `pinyin-isearch-chars--concat-variants'.
(defun pinyin-isearch-chars--add-full-fallback (string lvar)
  "Add full string to desiassembled variants.
If at the end of query there is unconvertable letters.  Global
variable `pinyin-isearch-strict' is used here.  Argument STRING
original request to add for fallback when strict mode is not
activated.
Argument LVAR dissasembled variants of characters for query."
  (when (and (not (string-empty-p string))
             pinyin-isearch-full-fallback
             (not pinyin-isearch-strict))
    (push (mapcar #'list (mapcar #'char-to-string (string-to-list string)))
          lvar))
    lvar)


(defun pinyin-isearch-chars--maptree (f l)
  "Recursively apply F to every leaf in tree/list L."
  (if (listp l)
      (mapcar (lambda (x) (pinyin-isearch-chars--maptree f x)) l)
    (funcall f l)))

(defun pinyin-isearch-chars--tree-remove-leaf (leaf l)
  "Remove all occurrences of LEAF from tree/list L."
  (cond
   ((equal l leaf) nil)
   ((listp l)
    (delq nil (mapcar (lambda (x) (pinyin-isearch-chars--tree-remove-leaf leaf x)) l)))
   (t l)))

;; (pinyin-isearch-chars--tree-remove-leaf
;;  "i"
;;  '((("奥澳傲熬敖凹袄懊坳嗷拗鏖骜鳌翱岙廒遨獒聱媪螯鏊" "昂肮盎" )) nil)) ; => ((("奥澳傲熬敖凹袄懊坳嗷拗鏖骜鳌翱岙廒遨獒聱媪螯鏊" "昂肮盎")))

;; (pinyin-isearch-chars--tree-remove-leaf
;;  "昂肮盎"
;;  '((("奥澳傲熬敖凹袄懊坳嗷拗鏖骜鳌翱岙廒遨獒聱媪螯鏊" "昂肮盎" )) "昂肮盎")) ; => ((("奥澳傲熬敖凹袄懊坳嗷拗鏖骜鳌翱岙廒遨獒聱媪螯鏊")))


;; Faster variant:
(defun pinyin-isearch-chars--is-here-p (orig-str sac)
  "Check that we have any leaf in SAC equal ORIG-STR."
  (cl-some (lambda (x) (when (string-equal x orig-str) t))
            (flatten-tree sac)))

;; ;; Slower variant:
;; (require 'seq)
;; (defun pinyin-isearch-chars--is-here-p (orig-str sac)
;;   "Check that we have any leaf in SAC equal ORIG-STR."
;;   (seq-some (lambda (x) (string-equal x orig-str))
;;             (flatten-tree sac)))


(defun pinyin-isearch-chars--convert-to-hieroglyphs (list-of-variants)
  "For every leaf of splitted request apply converter to hieroglyphs.
Argument LIST-OF-VARIANTS list that is result of function
`pinyin-isearch-chars--recursion'."
  (pinyin-isearch-chars--maptree #'pinyin-isearch-chars--pinyin-to-hieroglyphs list-of-variants))


;; Behavior with examples:
;; - `lch = (("昂") ("肮") ("盎"))` → "昂肮盎" (no brackets, all single char)
;; - `lch = (("昂肮"))` → "[昂肮]"
;; - `lch = (("昂" "肮"))` → "[昂肮]"
;; - `lch = (("X" "Y"))` → "[XY]"
;; - Marker case (`pinyin-isearch-chars--non-syllable-marker-number`, suppose it is ?\x1c):
;;     - `lch = (("\x1c昂"))` → "昂" (drop marker, no brackets)
;;     - `lch = (("\x1c肮" "\x1c盎"))` → "肮盎"

(defun pinyin-isearch-chars--regex-concat-hieroglyphs (lch)
  "Concatenate groups of Chinese characters for pinyin regex search.

LCH is a list of lists of strings.  Each inner list represents a group
 of characters, optionally prefixed by
 `pinyin-isearch-chars--non-syllable-marker-number' to disable
 bracketing.

For each group:
- Strings starting with the marker are stripped and not surrounded by brackets.
- If all strings in a group are marked, concatenate them without brackets.
- If the group contains only one unmarked string of length 1,
 concatenate it with marked strings, no bracket.
- Otherwise, concatenate all unmarked strings inside brackets '[...]',
 then append marked strings.

Returns the concatenated result as a single string.
Example:
  ((\"昂\" \"肮\" \"盎\") (\",词\")) => \"[昂肮盎]词\""
  (let ((marker pinyin-isearch-chars--non-syllable-marker-number))
    (mapconcat
     (lambda (inner)
       (let* ((marked (delq nil (mapcar (lambda (s)
                                          (if (and (> (length s) 0) (eq (elt s 0) marker))
                                              (substring s 1)
                                            nil))
                                        inner)))
              (unmarked (delq nil (mapcar (lambda (s)
                                            (if (or (not (> (length s) 0))
                                                    (not (eq (elt s 0) marker)))
                                                s
                                              nil))
                                          inner))))
         (cond
          ;; All marked: concat, no brackets
          ((and (> (length marked) 0)
                (= (length marked) (length inner)))
           (apply #'concat marked))
          ;; Only one unmarked, single char: no bracket
          ((and (= (length unmarked) 1)
                (= (length (car unmarked)) 1))
           (concat (car unmarked) (apply #'concat marked)))
          ;; Otherwise: bracket unmarked, append marked
          (t
           (concat "[" (apply #'concat unmarked) "]" (apply #'concat marked))))))
     lch "")))


(defun pinyin-isearch-chars--concat-variants (orig-str sac)
  "Construct regex alternation for pinyin variants.
ORIG-STR is user input; SAC is list of variant syllable lists (nested lists).

Returns a regex string with all disassembled variants, optionally
including the original pinyin if `pinyin-isearch-full-fallback' is true.

- Removes the original from variant list unless fallback.
- If SAC is nil or empty, returns impossible match regex."
  ;; Step 1: Mark the original pinyin string as required
  (let* ((marked-str (concat pinyin-isearch-chars--non-syllable-marker-string orig-str))
         ;; Step 2: Remove the marked original from variant tree
         (filtered-sac (if sac
                           (pinyin-isearch-chars--tree-remove-leaf marked-str sac)
                         nil)))
    ;; Step 3: Fallback handling, add original as variant if needed
    (when (and filtered-sac pinyin-isearch-full-fallback)
      (let ((first-elem (car filtered-sac)))
        (cond
         ;; Deeply nested: the structure is sac = ((("a" "b") ...)), so add to first branch
         ((and (consp first-elem)
               (consp (car first-elem))
               (stringp (car (car first-elem))))
          (push (list (list marked-str)) filtered-sac)))))
    ;; Step 4: Generate regex alternations
    (let ((variants
           (if (not filtered-sac)
               (if pinyin-isearch-full-fallback
                    (list orig-str)
                 ;; else
                 (list "$^"))  ;; Impossible match if no variants remain
             ;; else process
             (mapcar #'pinyin-isearch-chars--regex-concat-hieroglyphs filtered-sac))))
      ;; Step 5: Format final regex alternation
      (if (> (length variants) 1)
          (concat "\\("
                  (string-join variants "\\|")
                  "\\)")
        (car variants)))))


(defvar-local pinyin-isearch-chars--cached-query nil
  "For `pinyin-isearch-chars-regexp-function'.")
(defvar-local pinyin-isearch-chars--cached-regex nil
  "For `pinyin-isearch-chars-regexp-function'.")
(defvar-local pinyin-isearch-chars--cached-strict-flag nil
  "Non-nil means values saved for `pinyin-isearch-chars-regexp-function'.")
(defvar-local pinyin-isearch-chars--cached-fallback-flag nil
  "Non-nil means values saved for `pinyin-isearch-chars-regexp-function'.")
(defvar-local pinyin-isearch-chars--cached-full-fallback-flag nil
  "Non-nil means values saved for `pinyin-isearch-chars-regexp-function'.")



(defun pinyin-isearch-chars-regexp-function (string &optional _lax)
  "Replacement for function `isearch-regexp-function'.
Input - pinyin, output - regex for isearch.
If Variable `pinyin-isearch-strict' is set it uses strict version.
Step:
1) split to parts according to pinyin.
2) filter variants that do not endings without pinyin.
3) convert every syllable to hierogliphs.
4) surround variants of syllables with [], concat hieroglyphs and
concat variants with \\|.
Argument STRING isearch user input string of query.
Optional argument LAX (not used) used for isearch special cases."
  (unless pinyin-isearch-chars--first-syllable-letters
    (user-error "Pinyin-isearch, (pinyin-isearch-chars-load) was not called"))
  ;; create references to saved values, if next call will be the same.
  (when (or (not (eq pinyin-isearch-chars--cached-strict-flag pinyin-isearch-strict))
            (not (eq pinyin-isearch-chars--cached-fallback-flag pinyin-isearch-chars-fallback))
            (not (eq pinyin-isearch-chars--cached-full-fallback-flag pinyin-isearch-full-fallback)))
    (setq pinyin-isearch-chars--cached-query nil
          pinyin-isearch-chars--cached-regex nil))

  (when (not (string-equal string pinyin-isearch-chars--cached-query))
    (setq pinyin-isearch-chars--cached-query string)
    (setq pinyin-isearch-chars--cached-strict-flag pinyin-isearch-strict)
    (setq pinyin-isearch-chars--cached-fallback-flag pinyin-isearch-chars-fallback)
    (setq pinyin-isearch-chars--cached-full-fallback-flag pinyin-isearch-full-fallback)
    (setq pinyin-isearch-chars--cached-regex
          (pinyin-isearch-chars--concat-variants
           string
           (pinyin-isearch-chars--maptree ;; 2) convert-to-hieroglyphs - for nil return nil, or '((("嗯唔")))
            #'pinyin-isearch-chars--pinyin-to-hieroglyphs
            (pinyin-isearch-chars--recursion string)))) ;; 1) split - may return nil
    (when (or (not pinyin-isearch-chars--cached-regex)
              (string-empty-p pinyin-isearch-chars--cached-regex))
      (setq pinyin-isearch-chars--cached-regex "$^")))
  pinyin-isearch-chars--cached-regex)

(defun pinyin-isearch-chars-strict-regexp-function (string &optional lax)
  "Function `isearch-regexp-function' with strict mode.
This version of function set `pinyin-isearch-strict' enabled for
time of call.  Argument STRING isearch user input string of
query.  Optional argument LAX (not used) used for isearch special
cases."
  (let ((pinyin-isearch-strict t)
        (pinyin-isearch-chars-fallback nil)
        (pinyin-isearch-full-fallback nil))
    (pinyin-isearch-chars-regexp-function string lax)))


(provide 'pinyin-isearch-chars)
;;; pinyin-isearch-chars.el ends here
