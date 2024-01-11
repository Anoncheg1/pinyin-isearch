;;; pinyin-isearch-loaders.el --- Loaders of pinyin and hierogliphs from guail.  -*- lexical-binding: t -*-

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
;; Used to load "quail/PY.el" and "quail/Punct.el".

;; The problem it that data defined as arguments to call macro
;; `quail-define-rules'. We use advice to catch this argument.

;;; Code:

;; ---------- tools -------------------

(defvar pinyin-isearch--rules nil "Used in advice.")

(defun pinyin-isearch--quail-define-rules-advice (&rest rules)
  "Executed before `quail-define-rules' to catch passed arguments.
Optional argument ARGS catched rules argument."
  `(setq pinyin-isearch--rules ',rules)
)

(defun pinyin-isearch--quail-extractor (quail-file)
  "Used to set variable `pinyin-isearch--punct-rules'.
Argument QUAIL-FILE \"quail/PY.el\" for example."
  (advice-add 'quail-define-rules :override #'pinyin-isearch--quail-define-rules-advice)
  (load-file (locate-file quail-file load-path))
  (advice-remove 'quail-define-rules #'pinyin-isearch--quail-define-rules-advice)
  ;; return
  pinyin-isearch--rules)

(defun pinyin-isearch--punct-quail-filter (rules)
  "Load RULES for single letters of punctuations."
   (seq-filter (lambda (x) (= (length (car x)) 1)) rules))


;; ---------- load quail/PY.el for chinese hierogliphs ---------

(defun pinyin-isearch--py-rules-loader ()
  (let ((rul (pinyin-isearch--quail-extractor "quail/PY.el")))
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
      rul)
  )

(defconst pinyin-isearch--py-rules
  (pinyin-isearch--py-rules-loader)
  "Rules in form: ((\"a\" \"阿啊呵腌嗄锕吖\") (\"ai\" \"爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹\")...")


;; ---------- load punct and concatenate: py + punct --------

(defconst pinyin-isearch--punct-rules
  (pinyin-isearch--punct-quail-filter
   (pinyin-isearch--quail-extractor "quail/Punct.el"))
  "Extracted and filtered Chinese punctuation.")

(defconst pinyin-isearch--py-punct-rules
  (append pinyin-isearch--py-rules pinyin-isearch--punct-rules)
  "Extracted quail/PY.el + quail/Punct.el - Chinese heieroglyphs and punctuation.")


;; ---------- load pinyin from "quail/sisheng"  --------

;; We don't use result, we need only loaded variables
;; `sisheng-regexp', `sisheng-vowel-table', `sisheng-syllable-table'.
(defun pinyin-isearch--quail-make-sisheng-rules-advice (syllable)
  "We suppress function `quail-make-sisheng-rules.'
From quail/sisheng.el, for speed."
  nil)

(advice-add 'quail-make-sisheng-rules :override #'pinyin-isearch--quail-make-sisheng-rules-advice)
(pinyin-isearch--quail-extractor "quail/sisheng.el")
(advice-remove 'quail-make-sisheng-rules #'pinyin-isearch--quail-make-sisheng-rules-advice)

(provide 'pinyin-isearch-loaders)
;;; pinyin-isearch-loaders.el ends here
