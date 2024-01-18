;;; pinyin-isearch-loaders.el --- Loaders of pinyin and Chinese characters from guail  -*- lexical-binding: t -*-

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
;; Used to locate and load "chinese-sisheng", "chinese-py",
;; "chinese-punct".


;; The problem it that data defined as arguments to call macro
;; `quail-define-rules'.  We use advice to catch this argument.

;;; Code:

;; ---------- tools -------------------

(defvar pinyin-isearch-loaders--rules nil "Used in advice.")

(defun pinyin-isearch-loaders--get-location-of-input-method (leim-name)
  "Get elisp file location.
Argument LEIM-NAME input-method name."
  (car (nthcdr 5 (assoc leim-name input-method-alist))))

(defun pinyin-isearch-loaders--quail-define-rules-advice (&rest rules)
  "Replace `quail-define-rules' to catch passed arguments.
Optional argument ARGS catched RULES argument."
  `(setq pinyin-isearch-loaders--rules ',rules))

(defun pinyin-isearch-loaders--quail-define-package-advice (&rest args)
  "Replace `quail-define-package' to disable it.
Argument ARGS not used."
  (setq args args) ; suppress Warning: Unused lexical argument `args'
  )

(defun pinyin-isearch-loaders--quail-defrule-advice (&rest args)
  "Replace `quail-defrule' to disable it.
Argument ARGS not used."
  (setq args args) ; suppress Warning: Unused lexical argument `args'
  )


(defun pinyin-isearch-loaders--quail-extractor (quail-file)
  "Used to set variable `pinyin-isearch-loaders--punct-rules'.
Argument QUAIL-FILE \"quail/PY.el\" for example."
  (advice-add 'quail-define-rules :override #'pinyin-isearch-loaders--quail-define-rules-advice)
  (advice-add 'quail-define-package :override #'pinyin-isearch-loaders--quail-define-package-advice)
  (advice-add 'quail-defrule :override #'pinyin-isearch-loaders--quail-defrule-advice)
  (load (concat (pinyin-isearch-loaders--get-location-of-input-method quail-file) ".el"))
  (advice-remove 'quail-define-rules #'pinyin-isearch-loaders--quail-define-rules-advice)
  (advice-remove 'quail-define-package #'pinyin-isearch-loaders--quail-define-package-advice)
  (advice-remove 'quail-defrule #'pinyin-isearch-loaders--quail-defrule-advice)
  ;; return
  pinyin-isearch-loaders--rules)

(defun pinyin-isearch-loaders--punct-quail-filter (rules)
  "Load RULES for single letters of punctuations."
   (seq-filter (lambda (x) (= (length (car x)) 1)) rules))


;; ---------- load quail/PY.el for chinese hierogliphs ---------

(defun pinyin-isearch-loaders--py-rules-loader ()
  "Load quail rules and add lv and nv to lu and nu.
Because ǚ and other u tones is very same and with same letter."
  (let ((rul (pinyin-isearch-loaders--quail-extractor "chinese-py")))
      ;; remove v letter from pinyin
      ;; remove lv
      (setf (cadr (assoc-string "lu" rul))
            (concat (cadr (assoc-string "lv" rul))
                    (cadr (assoc-string "lu" rul))))
      ;; (setq rul (remove (assoc-string "lv" rul) rul))
      ;; remove nv
      (setf (cadr (assoc-string "nu" rul))
            (concat (cadr (assoc-string "nv" rul))
                    (cadr (assoc-string "nu" rul))))
      ;; (setq rul (remove (assoc-string "nv" rul) rul))
      rul))

(defconst pinyin-isearch-loaders--py-rules
  (pinyin-isearch-loaders--py-rules-loader)
  "Rules in form: ((\"a\" \"阿啊呵腌嗄锕吖\") (\"ai\" \"爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹\")...")


;; ---------- load punct and concatenate: py + punct --------

(defconst pinyin-isearch-loaders--punct-rules
  (pinyin-isearch-loaders--punct-quail-filter
   (pinyin-isearch-loaders--quail-extractor "chinese-punct"))
  "Extracted and filtered Chinese punctuation.")

(defconst pinyin-isearch-loaders--py-punct-rules
  (append pinyin-isearch-loaders--py-rules pinyin-isearch-loaders--punct-rules)
  "Extracted quail/PY.el + quail/Punct.el - Chinese heieroglyphs and punctuation.")


;; ---------- load pinyin from "quail/sisheng"  --------

;; We don't use result, we need only loaded variables
;; `sisheng-regexp', `sisheng-vowel-table', `sisheng-syllable-table'.
(defun pinyin-isearch-loaders--quail-make-sisheng-rules-advice (syllable)
  "Suppress function `quail-make-sisheng-rules'.
From quail/sisheng.el, for speed.
Argument SYLLABLE not used."
  (setq syllable syllable) ; suppress warning: Unused lexical argument
  nil)

(advice-add 'quail-make-sisheng-rules :override #'pinyin-isearch-loaders--quail-make-sisheng-rules-advice)
(pinyin-isearch-loaders--quail-extractor "chinese-sisheng")
(advice-remove 'quail-make-sisheng-rules #'pinyin-isearch-loaders--quail-make-sisheng-rules-advice)

(provide 'pinyin-isearch-loaders)
;;; pinyin-isearch-loaders.el ends here
