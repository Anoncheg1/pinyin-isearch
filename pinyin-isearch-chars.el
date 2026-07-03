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

;; Steps:
;; 1. in loop find syllables in 0-6 first letters.
;; 2. recursive call for right (left) part
;; In 1. if it is last letters than we use hungry search
;; 3. add syllable to every variant of right part at level 2)
;; 4. concat all found variants of dissasembly at level 3)

;; Global variable `pinyin-isearch-strict' strict last syllable to
;; only one variant of syllable and only full ony.  And don't allow
;; pinyin characters at the end that was not found in syllables.

;; Not whis function dont use `pinyin-isearch-chars-fallback'.
;; Make it - If `pinyin-isearch-chars-fallback' is t and final characters of ST pinyin is not full character we add it as a variant with or without full chinese characters pinyin.
;; this function  for call (pinyin-isearch-chars--recursion "nih")))) ; return:((("ni") ("huo" "hun" "hui" "huang" "huan" "huai" "hua" "hu" "hou" "hong" "heng" "hen" ...)))
;; Make it to return: ((("ni") ("h" "huo" "hun" "hui" "huang" "huan" "huai" "hua" "hu" "hou" "hong" "heng" "hen" ...)))
;; with "h" as a variant.
;; for call (pinyin-isearch-chars--recursion "n")))) ; it should return: ((("n" "nuo" "nue" "nuan" "nu" "nou" "nong" "niu" "ning" "nin" "nie" "niao" "niang" ...)))
;; with "n" as a variant.
;; (defun pinyin-isearch-chars--recursion (st)
;;   "Split ST into all valid pinyin syllable variants for isearch.
;; Return list of variants: each variant is a list of syllable lists.
;; Handles strict/non-strict mode and incomplete last syllables.
;; Uses globals:
;;   - `pinyin-isearch-strict'
;;   - `pinyin-isearch-chars--py-punct-rules'
;;   - `pinyin-isearch-chars--get-syllables-by-prefix'
;;   - `pinyin-isearch-chars--non-syllable-marker-string'"
;;   (let* ((results '())
;;          (maxlen (min (length st) 6)))
;;     (when (> (length st) 0)
;;       ;; Try every possible split
;;       (dotimes (pos maxlen)
;;         (let* ((end (1+ pos))
;;                (prefix (substring st 0 end))
;;                (syllables
;;                 ;; Last syllable and strictness matters
;;                 (if (and (= end (length st)) (not pinyin-isearch-strict))
;;                     (pinyin-isearch-chars--get-syllables-by-prefix prefix)
;;                   (let ((found (assoc-string prefix pinyin-isearch-chars--py-punct-rules)))
;;                     (if found (list (car found))))))
;;                (rest (substring st end)))
;;           (when syllables
;;             ;; Recursively process the remainder, build all combinations
;;             (if (> (length rest) 0)
;;                 (let ((rest-variants (pinyin-isearch-chars--recursion rest)))
;;                   (dolist (variant rest-variants)
;;                     (push (cons syllables variant) results)))
;;               ;; If nothing remains, this syllable starts a variant
;;               (push (list syllables) results))))))
;;     ;; If nothing valid found, handle marker or strict nil result
;;     (cond
;;      ((null results)
;;       (if pinyin-isearch-strict
;;           nil
;;         (list (list (list (concat pinyin-isearch-chars--non-syllable-marker-string st))))))
;;      (t
;;       (nreverse results)))))

;; (defun pinyin-isearch-chars--recursion (st)
;;   "Split ST into all valid pinyin syllable variants for isearch.
;; Return list of variants: each variant is a list of syllable lists.
;; If `pinyin-isearch-chars-fallback' is t and the final syllable is incomplete,
;; add its raw form as variant alongside completions.
;; Requires:
;;   - `pinyin-isearch-strict'
;;   - `pinyin-isearch-chars--py-punct-rules'
;;   - `pinyin-isearch-chars--get-syllables-by-prefix'
;;   - `pinyin-isearch-chars--non-syllable-marker-string'
;;   - `pinyin-isearch-chars-fallback'"
;;   (let* ((results '())
;;          (maxlen (min (length st) 6)))
;;     (when (> (length st) 0)
;;       ;; Try every possible split
;;       (dotimes (pos maxlen)
;;         (let* ((end (1+ pos))
;;                (prefix (substring st 0 end))
;;                (syllables
;;                 (if (and (= end (length st)) (not pinyin-isearch-strict))
;;                     (pinyin-isearch-chars--get-syllables-by-prefix prefix)
;;                   (let ((found (assoc-string prefix pinyin-isearch-chars--py-punct-rules)))
;;                     (if found (list (car found))))))
;;                (rest (substring st end)))
;;           (when syllables
;;             (if (> (length rest) 0)
;;                 (let ((rest-variants (pinyin-isearch-chars--recursion rest)))
;;                   (dolist (variant rest-variants)
;;                     (push (cons syllables variant) results)))
;;               ;; Nothing remains, this syllable starts a variant
;;               (push (list syllables) results))))))

;;     ;; Fallback for incomplete last syllable
;;     (let* ((last-syllable (and results (car (car (last results)))))
;;            (incomplete-last
;;             (and
;;              pinyin-isearch-chars-fallback
;;              ;; The search string didn't match any full syllable
;;              (or (null last-syllable) (not (assoc-string st pinyin-isearch-chars--py-punct-rules)))))
;;            (fallback-variant (and incomplete-last (list (list st)))))
;;       (cond
;;        ((null results)
;;         (if pinyin-isearch-strict
;;             nil
;;           (list (list (list (concat pinyin-isearch-chars--non-syllable-marker-string st))))))
;;        (t
;;         (nreverse
;;          (if fallback-variant
;;              (cons fallback-variant results)
;;            results)))))))

;; (defun pinyin-isearch-chars--recursion (st)
;;   "Split ST into all valid pinyin syllable variants for isearch.
;; Return list of variants: each variant is a list of syllable lists.
;; If `pinyin-isearch-chars-fallback' is t and the final syllable is incomplete,
;; add its raw form as variant alongside completions.
;; Requires:
;;   - `pinyin-isearch-strict'
;;   - `pinyin-isearch-chars--py-punct-rules'
;;   - `pinyin-isearch-chars--get-syllables-by-prefix'
;;   - `pinyin-isearch-chars--non-syllable-marker-string'
;;   - `pinyin-isearch-chars-fallback'
;; May return nil!"
;;   (print (list "recursion" st))
;;   (let* ((results '())
;;          (maxlen (min (length st) 6)))
;;     (when (> (length st) 0)
;;       ;; Try every possible split
;;       (dotimes (pos maxlen)
;;         (let* ((end (1+ pos))
;;                (prefix (substring st 0 end))
;;                (syllables
;;                 (if (and (= end (length st)) (not pinyin-isearch-strict))
;;                     (pinyin-isearch-chars--get-syllables-by-prefix prefix)
;;                   (let ((found (assoc-string prefix pinyin-isearch-chars--py-punct-rules)))
;;                     (if found (list (car found))))))
;;                (rest (substring st end)))
;;           (when syllables
;;             (if (> (length rest) 0)
;;                 (let ((rest-variants (pinyin-isearch-chars--recursion rest)))
;;                   (dolist (variant rest-variants)
;;                     (push (cons syllables variant) results)))
;;               ;; Nothing remains, this syllable starts a variant
;;               (push (list syllables) results))))))

;;     ;; Fallback for incomplete last syllable
;;     ;; NOTE: The fix is here: no fallback variant in strict mode!
;;     (print (list "test1" st results (length results)))
;;     (let* ((last-syllable (and results (car (car (last results)))))
;;            (incomplete-last
;;             (and
;;              (not pinyin-isearch-strict)      ;; <-- fix: fallback only if not strict
;;              pinyin-isearch-chars-fallback
;;              ;; The search string didn't match any full syllable
;;              (or (null last-syllable)
;;                  (not (assoc-string st pinyin-isearch-chars--py-punct-rules))
;;                  )))
;;            ;; (print (list "test2" st last-syllable results (car results)))
;;            (fallback-variant (and incomplete-last
;;                                   (not (string-equal (apply #'concat (flatten-list (car results))) st))
;;                                   (list (list (concat pinyin-isearch-chars--non-syllable-marker-string st))))))
;;       (cond
;;        ((null results)
;;         (when (and
;;                (not pinyin-isearch-strict)
;;                pinyin-isearch-chars-fallback)
;;           (list (list (list (concat pinyin-isearch-chars--non-syllable-marker-string st))))))
;;        (t
;;         ;; (print (list "fallback-variant" fallback-variant))
;;         (nreverse
;;          ;; (if fallback-variant
;;          ;;     (cons fallback-variant results)
;;          ;;   results)
;;          results
;;          ))
;;        ))))

;; cleanean - Excellent!
;; (defun pinyin-isearch-chars--recursion (st)
;;   "Split ST into all valid pinyin syllable variants for isearch.
;; Returns a list of variants: each variant is a list of syllable lists.
;; - If `pinyin-isearch-chars-fallback' is true and the final syllable is incomplete (and not strict),
;;   adds its raw form as variant.
;; - In strict mode, no fallback is applied.
;; If no match, returns nil (or only fallback variant if fallback is enabled)."
;;   (let ((results nil)
;;         (maxlen (min (length st) 6)))
;;     (when (> (length st) 0)
;;       (dotimes (i maxlen)
;;         (let* ((end (1+ i))
;;                (prefix (substring st 0 end))
;;                (sylls (if (and (= end (length st)) (not pinyin-isearch-strict))
;;                           (pinyin-isearch-chars--get-syllables-by-prefix prefix)
;;                         (let ((found (assoc-string prefix pinyin-isearch-chars--py-punct-rules)))
;;                           (when found (list (car found))))))
;;                (rest (substring st end)))
;;           (when sylls
;;             (if (> (length rest) 0)
;;                 ;; Recursively process remainder, prepend sylls
;;                 (dolist (variant (pinyin-isearch-chars--recursion rest))
;;                   (push (cons sylls variant) results))
;;               ;; Nothing left, variant is just this syllable
;;               (push (list sylls) results))))))
;;     ;; Fallback for incomplete input (only if fallback enabled, not strict, no match)
;;     (cond
;;      ((null results)
;;       (when (and (not pinyin-isearch-strict)
;;                  pinyin-isearch-chars-fallback
;;                  (> (length st) 0))
;;         (list (list (list (concat pinyin-isearch-chars--non-syllable-marker-string st))))))
;;      (t
;;       ;; Avoid duplicate fallback if already present
;;       (let ((last (car (car (last results)))))
;;         (if (and (not pinyin-isearch-strict)
;;                  pinyin-isearch-chars-fallback
;;                  (or (null last)
;;                      (not (assoc-string st pinyin-isearch-chars--py-punct-rules)))
;;                  (not (string-equal
;;                        (apply #'concat (flatten-list (car results))) st)))
;;             (nreverse (cons (list (list (concat pinyin-isearch-chars--non-syllable-marker-string st))) results))
;;           (nreverse results)))))))

;; (defun recursion-core (st)
;;   "Split ST into all valid pinyin syllable lists (no fallback)."
;;   (let* ((results nil)
;;          (maxlen (min (length st) 6)))
;;     (when (> (length st) 0)
;;       (dotimes (i maxlen)
;;         (let* ((end (1+ i))
;;                (prefix (substring st 0 end))
;;                (sylls
;;                 (if (and (= end (length st)) (not pinyin-isearch-strict))
;;                     (pinyin-isearch-chars--get-syllables-by-prefix prefix)
;;                   (let ((found (assoc-string prefix pinyin-isearch-chars--py-punct-rules)))
;;                     (when found (list (car found)))))
;;                 )
;;                (rest (substring st end)))
;;           (when sylls
;;             (if (> (length rest) 0)
;;                 (dolist (variant (recursion-core rest))
;;                   (push (cons sylls variant) results))
;;               (push (list sylls) results)))))
;;       results)))

;; Very good two functions: `recursion-core' and `recursion'
;; (defun recursion-core (st)
;;   "Recursively split ST into all valid pinyin syllable lists and fallback variants.
;; Does the actual pinyin splitting, recursion, and collects possible split variants
;; including fallback for the tail."
;;   (let* ((len (length st))
;;          (maxlen (min 6 len))
;;          (results nil))
;;     (when (> len 0)
;;       (dotimes (i maxlen)
;;         (let* ((end (1+ i))
;;                (prefix (substring st 0 end))
;;                (sylls (cond
;;                        ((or pinyin-isearch-strict (< end len))
;;                         (let ((found (assoc-string prefix pinyin-isearch-chars--py-punct-rules)))
;;                           (when found (list (car found)))))
;;                        (t
;;                         (pinyin-isearch-chars--get-syllables-by-prefix prefix))))
;;                (rest (substring st end)))
;;           (when sylls
;;             (let ((rest-variants (and (> (length rest) 0)
;;                                       (recursion-core rest))))
;;               ;; Always offer fallback for tail if enabled.
;;               (when (and pinyin-isearch-chars-fallback (> (length rest) 0))
;;                 (push (list sylls (list (concat pinyin-isearch-chars--non-syllable-marker-string rest)))
;;                       results))
;;               ;; Terminal segment: finished splitting.
;;               (when (zerop (length rest))
;;                 (push (list sylls) results))
;;               ;; Combine valid splits.
;;               (when rest-variants
;;                 (dolist (variant rest-variants)
;;                   (push (cons sylls variant) results))))))))
;;     results))

;; (defun recursion (st)
;;   "Return all valid pinyin splits (and fallback variants as needed) for ST.
;; calls `recursion-core`."
;;   (let ((results (recursion-core st)))
;;     (cond
;;      ;; Fallback: no splits, fallback allowed.
;;      ((and (not results)
;;            (not pinyin-isearch-strict)
;;            pinyin-isearch-chars-fallback
;;            (> (length st) 0))
;;       (list (list (concat pinyin-isearch-chars--non-syllable-marker-string st))))
;;      ;; Fallback: results, but not exact or punctuation rule.
;;      ((and results
;;            (not pinyin-isearch-strict)
;;            pinyin-isearch-chars-fallback
;;            (not (assoc-string st pinyin-isearch-chars--py-punct-rules))
;;            (not (string-equal (apply #'concat (flatten-list (car results))) st)))
;;       (cons (list (concat pinyin-isearch-chars--non-syllable-marker-string st)) results))
;;      ;; Normal.
;;      (t results))))

;; Very good two functions: `recursion-core' and `recursion' V2
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

(defun recursion (st)
  "Return all valid pinyin splits (and fallback variants) for string ST.
Recursively splits ST into syllable lists using `recursion-core`.  If no
 valid split and fallback is enabled, returns fallback variant prefixed
 with marker.
Full fallback added only if no syllable was found with
 `pinyin-isearch-chars--get-syllables-by-prefix` function or in
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
     ;; ((and results
     ;;       (not pinyin-isearch-strict)
     ;;       pinyin-isearch-chars-fallback
     ;;       (not (assoc-string st pinyin-isearch-chars--py-punct-rules))
     ;;       (not (string-equal (apply #'concat (flatten-list (car results))) st)))
     ;;  (cons (list (list (concat pinyin-isearch-chars--non-syllable-marker-string st))) results)
     ;;  )
     (t results))))

(defalias 'pinyin-isearch-chars--recursion #'recursion)
;; (defun pinyin-isearch-chars--recursion (st)
;;   "Return all valid pinyin syllable splits of ST for isearch.
;; Variants are lists of syllable lists.
;; - In strict mode, no fallback variant is added.
;; - If fallback enabled and input incomplete, add marked fallback variant."
;;   (let* ((results nil)
;;          (maxlen (min (length st) 6)))
;;     (when (> (length st) 0)
;;       (dotimes (i maxlen)
;;         (let* ((end (1+ i))
;;                (prefix (substring st 0 end))
;;                (sylls
;;                 ;; Last chunk & non-strict: get by prefix; else: lookup punctuation rule.
;;                 (if (and (= end (length st)) (not pinyin-isearch-strict))
;;                     (pinyin-isearch-chars--get-syllables-by-prefix prefix)
;;                   (let ((found (assoc-string prefix pinyin-isearch-chars--py-punct-rules)))
;;                     (when found (list (car found)))))
;;                 )
;;                (rest (substring st end)))
;;           (when sylls
;;             (if (> (length rest) 0)
;;                 ;; Recursively process remainder, prepend sylls to result.
;;                 (dolist (variant (pinyin-isearch-chars--recursion rest))
;;                   (push (cons sylls variant) results))
;;               ;; No remainder: variant is just this syllable.
;;               (push (list sylls) results))))))
;;     ;; Fallback handling
;;     (if (and (not results)
;;              (not pinyin-isearch-strict)
;;              pinyin-isearch-chars-fallback
;;              (> (length st) 0))
;;         (list (list (list (concat pinyin-isearch-chars--non-syllable-marker-string st))))
;;       ;; Else, add fallback variant only if needed.
;;       (let ((need-fallback
;;              (and (not pinyin-isearch-strict)
;;                   pinyin-isearch-chars-fallback
;;                   results
;;                   (not (assoc-string st pinyin-isearch-chars--py-punct-rules))
;;                   (not (string-equal (apply #'concat (flatten-list (car results))) st)))))
;;         (if need-fallback
;;             (nreverse (cons (list (list (concat pinyin-isearch-chars--non-syllable-marker-string st))) results))
;;           (nreverse results))))))

;; splitted good
;; (defun pinyin-isearch-chars--valid-syllables (str)
;;   "Get valid syllables for this prefix S."
;;   (let ((found (assoc-string str pinyin-isearch-chars--py-punct-rules)))
;;     (if found
;;         (list (car found))
;;       (unless pinyin-isearch-strict
;;         (pinyin-isearch-chars--get-syllables-by-prefix str)))))


;; (defun pinyin-isearch-chars--recursion (st)
;;   "Split ST into all valid pinyin syllable variants for isearch.
;; Returns a list of variants: each variant is a list of syllable lists.
;; If no match, returns nil (or only fallback variant if fallback is enabled)."
;;   (let ((results nil)
;;         (maxlen (min (length st) 6)))
;;     (dotimes (i maxlen)
;;       (let* ((end (1+ i))
;;              (prefix (substring st 0 end))
;;              (rest (substring st end))
;;              (sylls
;;                (if (= end (length st)) ; <-- This is the key line!
;;                    (pinyin-isearch-chars--get-syllables-by-prefix prefix)
;;                  (pinyin-isearch-chars--valid-syllables prefix))))
;;         (when sylls
;;           (if (> (length rest) 0)
;;               (dolist (variant (pinyin-isearch-chars--recursion rest))
;;                 (push (cons sylls variant) results))
;;             (push (list sylls) results)))))
;;     ;; fallback as before...
;;     (if (null results)
;;         (when (and (not pinyin-isearch-strict)
;;                    pinyin-isearch-chars-fallback
;;                    (> (length st) 0))
;;           (list (list (list (concat pinyin-isearch-chars--non-syllable-marker-string st)))))
;;       (nreverse results))))


;; fixed recursion for "gni"
;; (defun pinyin-isearch-chars--extract-syllables (prefix rest)
;;   "Extract syllable list for PREFIX, handling fallback and strict mode."
;;   ;; Returns list of syllables matching prefix
;;   (if (and (= (length rest) 0) (not pinyin-isearch-strict))
;;       (pinyin-isearch-chars--get-syllables-by-prefix prefix)
;;     (let ((found (assoc-string prefix pinyin-isearch-chars--py-punct-rules)))
;;       (when found (list (car found))))))

;; (defun pinyin-isearch-chars--recursion-variants (st &optional is-first)
;;   "Recursively split ST into pinyin syllable variants.
;; If IS-FIRST is t, allow first letter fallback as literal."
;;   (let ((results nil)
;;         (maxlen (min (length st) 6)))
;;     (when (> (length st) 0)
;;       (let ((match-found nil))
;;         (dotimes (i maxlen)
;;           (let* ((end (1+ i))
;;                  (prefix (substring st 0 end))
;;                  (rest (substring st end))
;;                  (sylls (pinyin-isearch-chars--extract-syllables prefix rest)))
;;             (when sylls
;;               (setq match-found t)
;;               (if (> (length rest) 0)
;;                   (dolist (variant (pinyin-isearch-chars--recursion-variants rest))
;;                     (push (cons sylls variant) results))
;;                 (push (list sylls) results)))))
;;         ;; If we found no valid syllable splits, and is-first, treat first letter as literal
;;         (when (and (not match-found) is-first)
;;           ;; Treat the first char as a literal, rest solved recursively
;;           (let* ((first (substring st 0 1))
;;                  (rest (substring st 1))
;;                  (tail (if (> (length rest) 0)
;;                            (pinyin-isearch-chars--recursion-variants rest)
;;                          nil)))
;;             (if tail
;;                 (dolist (v tail)
;;                   (push (cons (list first) v) results))
;;               (push (list (list first)) results)))))
;;       results)))

;; (defun pinyin-isearch-chars--recursion (st)
;;   "Split ST into all valid pinyin syllable variants for isearch."
;;   (let ((results (pinyin-isearch-chars--recursion-variants st t))) ; pass is-first=t
;;     (cond
;;      ((null results)
;;       (when (and (not pinyin-isearch-strict)
;;                  pinyin-isearch-chars-fallback
;;                  (> (length st) 0))
;;         (pinyin-isearch-chars--fallback-variant st)))
;;      (t
;;       ;; Avoid duplicate fallback if already present
;;       (let ((last (car (car (last results)))))
;;         (if (and (not pinyin-isearch-strict)
;;                  pinyin-isearch-chars-fallback
;;                  (or (null last)
;;                      (not (assoc-string st pinyin-isearch-chars--py-punct-rules)))
;;                  (not (string-equal
;;                        (apply #'concat (flatten-list (car results))) st)))
;;             (nreverse (cons (pinyin-isearch-chars--fallback-variant st) results))
;;           (nreverse results)))))))


;; (defun pinyin-isearch-chars--extract-syllables (prefix rest)
;;   (if (and (zerop (length rest)) (not pinyin-isearch-strict))
;;       (pinyin-isearch-chars--get-syllables-by-prefix prefix)
;;     (let ((found (assoc-string prefix pinyin-isearch-chars--py-punct-rules)))
;;       (when found (list (car found))))))

;; (defun pinyin-isearch-chars--recursion-variants (st &optional is-first)
;;   (let ((results nil)
;;         (len (length st)))
;;     (when (> len 0)
;;       (let ((match-found nil))
;;         ;; Try splitting prefixes, from longest to shortest.
;;         (dotimes (i (min 6 len))
;;           (let* ((end (1+ i))
;;                  (prefix (substring st 0 end))
;;                  (rest (substring st end))
;;                  (sylls (pinyin-isearch-chars--extract-syllables prefix rest)))
;;             (when sylls
;;               (setq match-found t)
;;               (if (> (length rest) 0)
;;                   (dolist (variant (pinyin-isearch-chars--recursion-variants rest))
;;                     (push (cons sylls variant) results))
;;                 (push (list sylls) results)))))
;;         ;; If no matches found and is-first, fallback to single character splits
;;         ;; (when (and (not match-found) is-first)
;;         ;;   (let ((first (substring st 0 1))
;;         ;;         (rest (substring st 1)))
;;         ;;     (if (> (length rest) 0)
;;         ;;         (dolist (v (pinyin-isearch-chars--recursion-variants rest))
;;         ;;           (push (cons (list first) v) results))
;;         ;;       (push (list (list first)) results))))
;;         )
;;     results)))

;; (defun pinyin-isearch-chars--recursion (st)
;;   (let ((results (pinyin-isearch-chars--recursion-variants st t)))
;;     (cond
;;      ((null results)
;;       (when (and (not pinyin-isearch-strict)
;;                  pinyin-isearch-chars-fallback
;;                  (> (length st) 0))
;;         (pinyin-isearch-chars--fallback-variant st)))
;;      (t
;;       (let ((last-split (car (car (last results)))))
;;         (if (and (not pinyin-isearch-strict)
;;                  pinyin-isearch-chars-fallback
;;                  (or (null last-split)
;;                      (not (assoc-string st pinyin-isearch-chars--py-punct-rules)))
;;                  (not (string-equal (apply #'concat (flatten-list (car results))) st)))
;;             (nreverse (cons (pinyin-isearch-chars--fallback-variant st) results))
;;           (nreverse results)))))))


;; (defun pinyin-isearch-chars--extract-syllables (prefix rest)
;;   "Return syllables or punctuation for PREFIX, depending on REST and strictness."
;;   (cond
;;    ((assoc-string prefix pinyin-isearch-chars--py-punct-rules) (list prefix))
;;    ((and (zerop (length rest)) (not pinyin-isearch-strict))
;;     (pinyin-isearch-chars--get-syllables-by-prefix prefix))
;;    (t nil)))

;; (defun pinyin-isearch-chars--fallback-variant (st)
;;   "Stub: fallback magic variant."
;;   (list (list (concat pinyin-isearch-chars--non-syllable-marker-string st))))

;; (defun pinyin-isearch-chars--extract-syllables (prefix rest)
;;   "Return syllables or punctuation for PREFIX."
;;   (cond
;;    ((assoc-string prefix pinyin-isearch-chars--py-punct-rules) (list prefix))
;;    (t
;;     (let ((matches (pinyin-isearch-chars--get-syllables-by-prefix prefix)))
;;       (when matches
;;         (list matches))))))

;; (defun pinyin-isearch-chars--recursion-variants (st)
;;   "Split ST into possible syllable sequences."
;;   (let ((results nil)
;;         (len (length st)))
;;     (when (> len 0)
;;       (dotimes (i (min 6 len))
;;         (let* ((end (1+ i))
;;                (prefix (substring st 0 end))
;;                (rest (substring st end))
;;                (sylls (pinyin-isearch-chars--extract-syllables prefix rest)))
;;           (when sylls
;;             (if (> (length rest) 0)
;;                 (dolist (variant (pinyin-isearch-chars--recursion-variants rest))
;;                   (push (cons sylls variant) results))
;;               (push (list sylls) results)))))
;;     results)))

;; (defun pinyin-isearch-chars--recursion (st)
;;   "Return possible splits for ST, including fallback if needed."
;;   (let ((results (pinyin-isearch-chars--recursion-variants st)))
;;     (cond
;;      ((null results)
;;       (when (and (not pinyin-isearch-strict)
;;                  pinyin-isearch-chars-fallback
;;                  (> (length st) 0))
;;         (pinyin-isearch-chars--fallback-variant st)))
;;      (t
;;       (let ((last-split (car (car (last results)))))
;;         (if (and (not pinyin-isearch-strict)
;;                  pinyin-isearch-chars-fallback
;;                  (or (null last-split)
;;                      (not (assoc-string st pinyin-isearch-chars--py-punct-rules)))
;;                  (not (string-equal (apply #'concat (flatten-list (car results))) st)))
;;             (nreverse (cons (pinyin-isearch-chars--fallback-variant st) results))
;;           (nreverse results)))))))

;; not full syllables for fallbkack
;; (defun pinyin-isearch-chars--recursion (st)
;;   "Split ST into all valid pinyin syllable variants for isearch.
;; Returns a list of variants: each variant is a list of syllable lists.
;; If no match, returns nil (or only fallback variant if fallback is enabled)."
;;   ;; Initialize results accumulator and max substring length (up to 6)
;;   (let ((results nil)
;;         (maxlen (min (length st) 6)))
;;     ;; Try all possible syllable prefixes (from 1 up to maxlen chars)
;;     (dotimes (i maxlen)
;;       (let* ((end (1+ i))                      ;; Current end index for prefix
;;              (prefix (substring st 0 end))     ;; Current prefix candidate
;;              (rest (substring st end))         ;; Remaining string after prefix
;;              ;; Get valid syllables for this prefix (always use the same lookup as orig)
;;              (sylls (pinyin-isearch-chars--valid-syllables prefix)))
;;         ;; If prefix matched valid syllables
;;         (when sylls
;;           ;; If there is more string after this syllable
;;           (if (> (length rest) 0)
;;               ;; Recursively try to split rest, and add this syllable to each resulting variant
;;               (dolist (variant (pinyin-isearch-chars--recursion rest))
;;                 (push (cons sylls variant) results))
;;             ;; If nothing left, store this syllable as a complete variant
;;             (push (list sylls) results)))))
;;     ;; If no variants found
;;     (if (null results)
;;         ;; Only add fallback if enabled and not strict and input non-empty
;;         (when (and (not pinyin-isearch-strict)
;;                    pinyin-isearch-chars-fallback
;;                    (> (length st) 0))
;;           (list (list (list (concat pinyin-isearch-chars--non-syllable-marker-string st)))))
;;       ;; If results found, just return them (reverse for order)
;;       (nreverse results))))

;; ;; have issue
;; (defun pinyin-isearch-chars--recursion (st)
;;   "Split ST into all valid pinyin syllable variants for isearch.
;; Split ST into all valid pinyin syllable variants for isearch.
;; Return list of variants: each variant is a list of syllable lists.

;; If `pinyin-isearch-chars-fallback' is t and the final syllable is incomplete,
;; add the partial syllable as the first element in the last syllables list.
;; Handles strict/non-strict mode and incomplete last syllables.

;; Uses globals:
;;   - `pinyin-isearch-strict'
;;   - `pinyin-isearch-chars--py-punct-rules'
;;   - `pinyin-isearch-chars--get-syllables-by-prefix'
;;   - `pinyin-isearch-chars--non-syllable-marker-string'"
;;   (let* ((results '())
;;          (maxlen (min (length st) 6)))
;;     (when (> (length st) 0)
;;       ;; Try every possible split
;;       (dotimes (pos maxlen)
;;         (let* ((end (1+ pos))
;;                (prefix (substring st 0 end))
;;                (syllables
;;                 (if (and (= end (length st)) (not pinyin-isearch-strict))
;;                     (pinyin-isearch-chars--get-syllables-by-prefix prefix)
;;                   (let ((found (assoc-string prefix pinyin-isearch-chars--py-punct-rules)))
;;                     (if found (list (car found))))))
;;                (rest (substring st end)))
;;           (when syllables
;;             (if (> (length rest) 0)
;;                 ;; Recursively process the remainder
;;                 (dolist (variant (pinyin-isearch-chars--recursion rest))
;;                   (push (cons syllables variant) results))
;;               ;; No remainder, start a variant
;;               (push (list syllables) results))))))

;;     ;; Fallback: merge partial syllable to front of last syllable list in last variant
;;     (cond
;;      ((null results)
;;       (if pinyin-isearch-strict
;;           nil
;;         (list (list (list (concat pinyin-isearch-chars--non-syllable-marker-string st))))))
;;      (t
;;       ;; Only apply if fallback is enabled, incomplete, and last syllable list is at the end
;;       (let* ((last-variant (car (last results)))
;;              (last-syllables (car (last last-variant)))
;;              ;; Is incomplete, i.e. `st` is not a full syllable in rules
;;              (partial
;;               (and pinyin-isearch-chars-fallback
;;                    (not pinyin-isearch-strict)
;;                    (> (length st) 0)
;;                    ;; Check for incomplete last syllable
;;                    (not (assoc-string st pinyin-isearch-chars--py-punct-rules)))))
;;         (if (and partial last-syllables)
;;             ;; Insert partial in the front of the last syllable list
;;             (append
;;              (butlast results 1)
;;              (list
;;               (append
;;                (butlast last-variant 1)
;;                (list (cons st last-syllables)))))
;;           (nreverse results)))))))



;; (defun pinyin-isearch-chars--recursion (st)
;;   "Split ST into all valid pinyin syllable variants for isearch.
;; Split ST into all valid pinyin syllable variants for isearch.
;; Return list of variants: each variant is a list of syllable lists.

;; If `pinyin-isearch-chars-fallback' is t and the final syllable is incomplete,
;; add the partial syllable as the first element in the last syllables list.
;; Handles strict/non-strict mode and incomplete last syllables.

;; Uses globals:
;;   - `pinyin-isearch-strict'
;;   - `pinyin-isearch-chars--py-punct-rules'
;;   - `pinyin-isearch-chars--get-syllables-by-prefix'
;;   - `pinyin-isearch-chars--non-syllable-marker-string'"
;;   (let* ((results '())
;;          (maxlen (min (length st) 6)))
;;     (when (> (length st) 0)
;;       ;; Try every possible split
;;       (dotimes (pos maxlen)
;;         (let* ((end (1+ pos))
;;                (prefix (substring st 0 end))
;;                (syllables
;;                 (if (and (= end (length st)) (not pinyin-isearch-strict))
;;                     (pinyin-isearch-chars--get-syllables-by-prefix prefix)
;;                   (let ((found (assoc-string prefix pinyin-isearch-chars--py-punct-rules)))
;;                     (if found (list (car found))))))
;;                (rest (substring st end)))
;;           (when syllables
;;             (if (> (length rest) 0)
;;                 ;; Recursively process the remainder
;;                 (dolist (variant (pinyin-isearch-chars--recursion rest))
;;                   (push (cons syllables variant) results))
;;               ;; No remainder, start a variant
;;               (push (list syllables) results))))))

;;     ;; Fallback: merge partial syllable to front of last syllable list in last variant
;;     (let ((result
;;            (cond
;;             ((null results)
;;              (if pinyin-isearch-strict
;;                  nil
;;                (list (list (list (concat pinyin-isearch-chars--non-syllable-marker-string st))))))
;;             (t
;;              ;; Only apply if fallback is enabled, incomplete, and last syllable list is at the end
;;              (when  pinyin-isearch-full-fallback
;;                (push (list (list st)) results))

;;              (let* ((last-variant (car (last results)))
;;                     (last-syllables (car (last last-variant)))
;;                     ;; Is incomplete, i.e. `st` is not a full syllable in rules
;;                     (partial
;;                      (and pinyin-isearch-chars-fallback
;;                           (> (length st) 0)
;;                           (not (assoc-string st pinyin-isearch-chars--py-punct-rules)))))
;;                (if (and partial last-syllables)
;;                    ;; Insert partial in the front of the last syllable list
;;                    (append
;;                     (butlast results 1)
;;                     (list
;;                      (append
;;                       (butlast last-variant 1)
;;                       (list (cons st last-syllables)))))
;;                  (nreverse results)))))))
;;       ;; PATCH: Always add a variant of full input ST if non-empty
;;       ;; (when (and (> (length st) 0) pinyin-isearch-full-fallback)
;;       ;;   (setq result (append result (list (list (list st))))))
;;       result)))

;; ;; not used
;; (defun pinyin-isearch-chars--filter-full-variants (f l)
;;   "Filter variants that has unfinished letters at the end.
;; Variants of disassemble.  Unfinished letters is that we we can
;;  not guess what Chinese charater it is.  If there is only
;;  variants with unfinished letters, we don't filter them.
;;  Function F is a function able convert pinyin to Chinese
;;  characters.  Steps: 1) filter variants ending with hieroglyphs
;;  2) return filtered varians or all if filtered is nil.  Argument
;;  L is a list of disassemble variants."
;;   (or
;;    ;; Alternative is `seq-filter'
;;    (cl-remove-if-not (lambda (x)
;;                  ;; get the last syllable variants
;;                  (let* ((last (car (nth (1- (length x)) x)))
;;                        (cres (funcall f last)))
;;                    ;; save which can be converted to Chinese, if not equal than it was converted.
;;                    (and cres ; just in case, but `pinyin-isearch-chars--pinyin-to-hieroglyphs` that is used
;;                         (not (equal cres last))))) ; result of predicate

;;                l)
;;    l))

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
    ;; (setq lvar (append lvar (list (list string))))
    (push (mapcar #'list (mapcar #'char-to-string (string-to-list string)))
          lvar))
    lvar)

;; (defun pinyin-isearch-chars--add-fallback (string lvar)
;;   "Add full string to desiassembled variants. Argument STRING original request, LVAR variants."
;;   ;; original fallback logic
;;   (if (and (not (string-empty-p string))
;;            pinyin-isearch-chars-fallback
;;            (not pinyin-isearch-strict)
;;            (or (> (length lvar) 1) (> (length (car lvar)) 1)))
;;       (let ((la (car (car (last (car (last lvar)))))))
;;         (if (eq (elt la 0) pinyin-isearch-chars--non-syllable-marker-number)
;;             ;; add full string (marked) to result list as another variant.
;;             (setq lvar (cons (list (list (concat pinyin-isearch-chars--non-syllable-marker-string string))) lvar)))))
;;   ;; PATCH: unconditionally add full input variant if string is not empty
;;   (when (and (not (string-empty-p string))
;;              pinyin-isearch-full-fallback)
;;     ;; (setq lvar (append lvar (list (list string))))
;;     (push  (mapcar #'list (mapcar #'char-to-string (string-to-list string)))
;;           lvar)
;;     )
;;   lvar)

;; (defun pinyin-isearch-chars--maptree (f l)
;;   "Apply map to every leaf of a list.
;; Argument F\(x\) function that will be applyed to leafs.
;; Argument L list with any structure of sublists.
;; Return original L with replaced leafs by result of F."
;;   (mapcar (lambda (x) (if (listp x)
;;                           (pinyin-isearch-chars--maptree f x)
;;                         ;; else
;;                         (funcall f x)))
;;           l))



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

;; (pinyin-isearch-chars--tree-remove-leaf
;;  "i"
;;  '((("奥澳傲熬敖凹袄懊坳嗷拗鏖骜鳌翱岙廒遨獒聱媪螯鏊" "昂肮盎" )) nil)) ; => ((("奥澳傲熬敖凹袄懊坳嗷拗鏖骜鳌翱岙廒遨獒聱媪螯鏊" "昂肮盎")))

;; (pinyin-isearch-chars--tree-remove-leaf
;;  "昂肮盎"
;;  '((("奥澳傲熬敖凹袄懊坳嗷拗鏖骜鳌翱岙廒遨獒聱媪螯鏊" "昂肮盎" )) "昂肮盎")) ; => ((("奥澳傲熬敖凹袄懊坳嗷拗鏖骜鳌翱岙廒遨獒聱媪螯鏊")))

(defun pinyin-isearch-chars--convert-to-hieroglyphs (list-of-variants)
  "For every leaf of splitted request apply converter to hieroglyphs.
Argument LIST-OF-VARIANTS list that is result of function
`pinyin-isearch-chars--recursion'."
  (pinyin-isearch-chars--maptree #'pinyin-isearch-chars--pinyin-to-hieroglyphs list-of-variants))

;; (defun pinyin-isearch-chars--regex-concat-hieroglyphs (lch)
;;   "Suround strings with [], like 昂肮盎 to [昂肮盎] and concat them.
;; Argument LCH is a list of lists with strings. Those strings inside of
;;  inner list are concatenated firstly and then surounded with [] and then
;;  concatenated.
;; If sub list have only one string consisting of a one character, such
;;  string should not be surounded with [].
;; Also, if the first character of string is
;;  `pinyin-isearch-chars--non-syllable-marker-number', then strings should
;;  not be surounded with [].
;; At higher level we work on one variant of syllable.
;; Return string."
;;   ;; if first character of string equel "", this mean that it should be placed without surounding with [ ]
;;   (if (cl-every (lambda (x)
;;                   (and (stringp (car x))
;;                        (= (length (car x)) 1)))
;;                 lch)
;;       ;; All single chars: just join
;;       (mapconcat (lambda (x) (car x)) lch "")
;;     ;; else
;;     (mapconcat (lambda (x)
;;                  ;; apply to every ("sd" "sd") or ("sd")
;;                  (let ((cx (car x)))
;;                    ;; (print (list "wtf" cx x))
;;                    (if (eq (length x) 1)

;;                        (if  ; first character equel ""
;;                            (eq (elt cx 0) pinyin-isearch-chars--non-syllable-marker-number)
;;                            (substring cx 1) ; delete first character
;;                          ;; else
;;                          (concat "[" cx "]"))
;;                      ;; else ("sd" "sd")
;;                      (concat "[" (apply #'concat x) "]"))))
;;                lch nil)))

;; (defun pinyin-isearch-chars--regex-concat-hieroglyphs (lch)
;;   "Suround strings with [], like 昂肮盎 to [昂肮盎] and concat them.

;; Argument LCH is a list of lists with strings. Those strings inside of
;;  inner list are concatenated firstly and then surounded with [] and then
;;  concatenated.
;; If sub list have only one string consisting of a one character, such
;;  string should not be surounded with [].
;; Marker at start strips and disables bracket, if the first character of
;;  string is `pinyin-isearch-chars--non-syllable-marker-number', then
;;  strings should not be surounded with [].
;; At higher level we work on one variant of syllable.
;; Return string."
;;   (let ((marker pinyin-isearch-chars--non-syllable-marker-number))
;;     (mapconcat
;;      (lambda (inner)
;;        ;; Strip marker from each string, and check if all had marker.
;;        (let* ((stripped
;;                (mapcar
;;                 (lambda (s)
;;                   (if (and (> (length s) 0)
;;                            (eq (elt s 0) marker))
;;                       (substring s 1)
;;                     s))
;;                 inner))
;;               (all-marked
;;                (and (> (length inner) 0)
;;                     (cl-every (lambda (s)
;;                                 (and (> (length s) 0)
;;                                      (eq (elt s 0) marker)))
;;                               inner))))
;;          (cond
;;           (all-marked (apply #'concat stripped)) ; marker disables brackets
;;           ((and (= (length stripped) 1)
;;                 (= (length (car stripped)) 1))
;;            (car stripped)) ;; single-char, no bracket
;;           (t (concat "[" (apply #'concat stripped) "]")))))
;;      lch nil)))

;; Behavior with examples:
;; - `lch = (("昂") ("肮") ("盎"))` → "昂肮盎" (no brackets, all single char)
;; - `lch = (("昂肮"))` → "[昂肮]"
;; - `lch = (("昂" "肮"))` → "[昂肮]"
;; - `lch = (("X" "Y"))` → "[XY]"
;; - Marker case (`pinyin-isearch-chars--non-syllable-marker-number`, suppose it is ?\x1c):
;;     - `lch = (("\x1c昂"))` → "昂" (drop marker, no brackets)
;;     - `lch = (("\x1c肮" "\x1c盎"))` → "肮盎"
;; (defun pinyin-isearch-chars--regex-concat-hieroglyphs (lch)
;;   "Suround strings with [], like 昂肮盎 to [昂肮盎] and concat them.

;; Argument LCH is a list of lists with strings. Those strings inside of
;;  inner list are concatenated firstly and then surounded with [] and then
;;  concatenated.
;; If sub list have only one string consisting of a one character, such
;;  string should not be surounded with [].
;; Marker at start strips and disables bracket, if the first character of
;;  string is `pinyin-isearch-chars--non-syllable-marker-number', then
;;  strings should not be surounded with [].
;; At higher level we work on one variant of syllable.
;; Return string."
;;   (let ((marker pinyin-isearch-chars--non-syllable-marker-number))
;;     (mapconcat
;;      (lambda (inner)
;;        ;; Process each string in inner separately
;;        (mapconcat
;;         (lambda (s)
;;           (let* ((is-marked (and (> (length s) 0)
;;                                  (eq (elt s 0) marker)))
;;                  (stripped (if is-marked (substring s 1) s)))
;;             (cond
;;              (is-marked stripped) ; Marker disables bracket
;;              ((and (= (length stripped) 1)) stripped) ; Single-char, no bracket
;;              (t (concat "[" stripped "]")))))
;;         inner
;;         "")) ; concat inner results
;;      lch "")))

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

;; (defun pinyin-isearch-chars--regex-concat-hieroglyphs (l)
;;   "Concatenate hieroglyphs for a regex.
;; If all are single-character, just concatenate.
;; Else, wrap entries in [].  Handles special marker.
;; Convert 昂肮盎 to [昂肮盎] and concat such strings.
;; This is done for every variant of syllable.
;; Argument L list of form ((\"gg\"))."
;;   (if (cl-every (lambda (x)
;;                   (and (stringp (car x))
;;                        (= (length (car x)) 1)))
;;                 l)
;;       ;; All single chars: just join
;;       (mapconcat (lambda (x) (car x)) l "")
;;     ;; Some are not, handle individually
;;     (mapconcat
;;      (lambda (x)
;;        (let ((cx (car x)))
;;          (cond
;;           ;; Special marker handling (as in your original code)
;;           ((and (stringp cx)
;;                 (> (length cx) 0)
;;                 (eq (elt cx 0) pinyin-isearch-chars--non-syllable-marker-number))
;;            (substring cx 1)) ; drops first char (marker)
;;           ;; Multichar string: wrap in []
;;           ((and (stringp cx) (> (length cx) 1))
;;            (concat "[" cx "]"))
;;           ;; Single char, in non-homogenous input: wrap in []
;;           (t (concat "[" cx "]")))))
;;      l "")))

;; (defun pinyin-isearch-chars--concat-variants (sac) ; Problem is here it filter "h"
;; "Create regex alternation for dissasemble variants.
;; Argument SAC is splitted-and-converted variants."
;; (if (> (length sac) 1)
;;     (concat "\\(" (mapconcat #'pinyin-isearch-chars--regex-concat-hieroglyphs
;;                              sac "\\|") "\\)")
;;   ;; else eq 1
;;   (pinyin-isearch-chars--regex-concat-hieroglyphs (car sac))))

;; (let ((v '(("\34a&&") (("昂肮盎" "昂肮盎") ("奥澳傲熬敖凹袄懊坳嗷拗鏖骜鳌翱岙廒遨獒聱媪螯鏊" "昂肮盎" "安案按暗岸俺谙黯鞍氨庵桉鹌胺铵揞犴埯" "爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹" "阿啊呵腌嗄锕吖")))))
;;   ;; (listp (car (car v))))
;;   (mapcar (lambda (x) (if (stringp (car x))
;;                           (pinyin-isearch-chars--regex-concat-hieroglyphs x)
;;                           ))
;;           v)
;;   (push "inserted" (car (car v)))
;;   v)

;; (let ((v '((("\34a&&")) (("ss" "ss") ("\34&&")))))
;;   (mapcar #'pinyin-isearch-chars--regex-concat-hieroglyphs v))
;; ("\34a&&")

;; (defun pinyin-isearch-chars--concat-variants (orig-str sac) ; Problem is here it filter "h"
;;   "Create regex alternation for dissasemble variants.
;; Argument ORIG-STR is original pinyin provided by user.
;; Argument SAC is splitted-and-converted variants.
;; SAC may be a list of list of syllable lists or
;;  nil, but not mix of them."
;;   ;; input: sac=((("女钕恧衄怒努奴弩驽胬孥")))
;;   (print (list "AAAAAAAAAAAAAA" sac (length sac) (car (last sac))))
;;   (let ((orig-element-marked (concat pinyin-isearch-chars--non-syllable-marker-string
;;                                      orig-str))
;;         processed-sac)


;;     ;; fallback handling
;;     (when sac
;;       (setq sac (pinyin-isearch-chars--tree-remove-leaf orig-element-marked sac))
;;       (when pinyin-isearch-full-fallback
;;         (if (stringp (caaar sac))
;;             ;; add element
;;             (push orig-element-marked (car (car sac)))
;;           ;; else
;;           (when (stringp (caar sac))
;;             (push orig-element-marked  (car sac))))))

;;     (setq processed-sac (if (not sac)
;;                        (list "$^")
;;                      ;; else
;;                      (mapcar #'pinyin-isearch-chars--regex-concat-hieroglyphs sac2)
;;                      ))

;;     (print (list "Assssssss processed-sac= sac2=" processed-sac sac2 (car (last sac2))))

;;     ;; unpack
;;     (if (> (length processed-sac) 1)
;;         (concat "\\("
;;                 (string-join processed-sac "\\|")
;;                 "\\)")
;;       ;; else
;;       (car processed-sac))))

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
         ;; ;; Less nested: sac = (("a" "b")), so add to first element
         ;; ((and (consp first-elem)
         ;;       (stringp (car first-elem)))
         ;;  (push marked-str filtered-sac)))))
    ;; (print (list "ZZ N1 filtered-sac=" filtered-sac))
    ;; Step 4: Generate regex alternations
    (let ((variants
           (if (not filtered-sac)
               (if pinyin-isearch-full-fallback
                    (list orig-str)
                 ;; else
                 (list "$^"))  ;; Impossible match if no variants remain
             ;; else process
             (mapcar #'pinyin-isearch-chars--regex-concat-hieroglyphs filtered-sac))))
      ;; (print (list "ZZ N2 variants=" variants))
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



;; (defmacro pinyin-isearch-chars--impossible-regex (variable)
;;   "Replace string with impossible regex to abort isearch.
;; Didn't find better approach yet.
;; Argument VARIABLE variable with string."
;;   `(if (equal ,variable "")
;;           "$^"
;;         ;; else
;;         ,variable)) ; impossible regex - to abort search


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
  ;; (print (list "pinyin-isearch-chars-regexp-function" string))
  (unless pinyin-isearch-chars--first-syllable-letters
    (user-error "Pinyin-isearch, (pinyin-isearch-chars-load) was not called"))
  ;; create references to saved values, if next call will be the same.
  (when (or (not (eq pinyin-isearch-chars--cached-strict-flag pinyin-isearch-strict))
            (not (eq pinyin-isearch-chars--cached-fallback-flag pinyin-isearch-chars-fallback))
            (not (eq pinyin-isearch-chars--cached-full-fallback-flag pinyin-isearch-full-fallback)))
    (setq pinyin-isearch-chars--cached-query nil
          pinyin-isearch-chars--cached-regex nil))

  (when (not (string-equal string pinyin-isearch-chars--cached-query))
    ;; (print (list "pinyin-isearch-chars-regexp-function N1 " string pinyin-isearch-chars--cached-query pinyin-isearch-chars--cached-regex))

    ;; (print (list "pinyin-isearch-chars-regexp-function N11 "
    ;;         ;; (pinyin-isearch-chars--concat-variants
    ;;        ;; (pinyin-isearch-chars--add-full-fallback
    ;;        ;;  string
    ;;         ;; splitted and converted after it:
    ;;         ;; (pinyin-isearch-chars--convert-to-hieroglyphs
    ;;          ;; (pinyin-isearch-chars--add-fallback
    ;;          ;;  string
    ;;           ;; apply filter
    ;;           ;; (pinyin-isearch-chars--filter-full-variants ; 2) Filter variants that has unfinished letters at the end.
    ;;           ;;  #'pinyin-isearch-chars--pinyin-to-hieroglyphs
    ;;            ;; split to variants
    ;;            (pinyin-isearch-chars--recursion string)))
    ;; (print (list "pinyin-isearch-chars-regexp-function N12 "
    ;;              (pinyin-isearch-chars--maptree ; convert-to-hieroglyphs
    ;;               #'pinyin-isearch-chars--pinyin-to-hieroglyphs
    ;;               ;; (pinyin-isearch-chars--add-fallback
    ;;               ;;  string
    ;;               ;; apply filter
    ;;               ;; (pinyin-isearch-chars--filter-full-variants ; 2) Filter variants that has unfinished letters at the end.
    ;;               ;;  #'pinyin-isearch-chars--pinyin-to-hieroglyphs
    ;;               ;; split to variants
    ;;               (pinyin-isearch-chars--recursion string))))
    (setq pinyin-isearch-chars--cached-query string)
    (setq pinyin-isearch-chars--cached-strict-flag pinyin-isearch-strict)
    (setq pinyin-isearch-chars--cached-fallback-flag pinyin-isearch-chars-fallback)
    (setq pinyin-isearch-chars--cached-full-fallback-flag pinyin-isearch-full-fallback)
    (setq pinyin-isearch-chars--cached-regex
          (pinyin-isearch-chars--concat-variants
           ;; (pinyin-isearch-chars--add-full-fallback
           string
           (pinyin-isearch-chars--maptree ;; 2) convert-to-hieroglyphs - for nil return nil, or '((("嗯唔")))
            #'pinyin-isearch-chars--pinyin-to-hieroglyphs
            ;; (pinyin-isearch-chars--filter-full-variants ; 2) Filter variants that has unfinished letters at the end.
            ;;  #'pinyin-isearch-chars--pinyin-to-hieroglyphs
            (pinyin-isearch-chars--recursion string) ;; 1) split - may return nil
            )))
    ;; (print (list "pinyin-isearch-chars-regexp-function N2 " string pinyin-isearch-chars--cached-query pinyin-isearch-chars--cached-regex))
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
