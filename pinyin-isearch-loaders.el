;;; pinyin-isearch-loaders.el --- Loaders of pinyin and Chinese characters from guail  -*- lexical-binding: t -*-

;; Copyright (c) 2024-2026 Anoncheg1

;; Author: Anoncheg1
;; Keywords: chinese, pinyin, matching, convenience
;; URL: https://github.com/Anoncheg1/pinyin-isearch
;; Version: 1.7.2
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; License

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not,
;; see <https://www.gnu.org/licenses/agpl-3.0.en.html>.

;;; Commentary:
;; Used to locate and load "chinese-sisheng", "chinese-py",
;; "chinese-punct".


;; The problem it that data defined as arguments to call macro
;; `quail-define-rules'.  We use advice to catch this argument.

;;; Code:

;; ---------- tools -------------------
(require 'seq) ; for `seq-filter'

(defvar pinyin-isearch-loaders--rules nil "Used in advice.")

(defun pinyin-isearch-loaders--get-location-of-input-method (leim-name)
  "Return Quail package file location for input method LEIM-NAME.
Signals a single error if LEIM-NAME is not found, or if the entry
is not a valid Quail method with a package name."
  (let ((entry (assoc leim-name input-method-alist)))
    (unless (and entry
                 (>= (length entry) 6)
                 (eq (nth 2 entry) 'quail-use-package))
      (error "Cannot determine Quail file for `%s'" leim-name))
    (nth 5 entry)))


(defun pinyin-isearch-loaders--quail-define-rules-advice (&rest rules)
  "Replace `quail-define-rules' to catch passed arguments.
Optional argument ARGS catched RULES argument."
  `(setq pinyin-isearch-loaders--rules ',rules))


(defun pinyin-isearch-loaders--quail-extractor (quail-file)
  "Used to set variable `pinyin-isearch-loaders--punct-rules'.
Argument QUAIL-FILE \"quail/PY.el\" for example."

  ;; Advices here used for speed and memory optimization.
  ;; IIRC some other package faced a similar problem, we should `M-x
  ;; report-emacs-bug' and ask for an easier way to access this data (maybe
  ;; it can be reconstructed from the input-method table, but either way
  ;; Emacs should provide that).
  (unwind-protect ;; Remove the advices even in case of an error!
      (let (pinyin-isearch-loaders--rules)
        (advice-add 'quail-define-rules :override
                    #'pinyin-isearch-loaders--quail-define-rules-advice)
        ;; (advice-add 'quail-define-package :override #'ignore) ; used for speed optimization
        ;; (advice-add 'quail-defrule :override #'ignore)
        (load (concat (pinyin-isearch-loaders--get-location-of-input-method quail-file) ".el"))
        ;; return
        pinyin-isearch-loaders--rules)
    (advice-remove 'quail-define-rules #'pinyin-isearch-loaders--quail-define-rules-advice)
    ;; (advice-remove 'quail-define-package #'ignore)
    ;; (advice-remove 'quail-defrule #'ignore)
    ))

(defun pinyin-isearch-loaders--punct-quail-filter (rules)
  "Load RULES for single letters of punctuations."
  (seq-filter (lambda (x) (length= (car x) 1)) rules))


;; ---------- load quail/PY.el for chinese hieroglyphs ---------

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

;; ---------- load pinyin from "quail/sisheng"  --------

(defun pinyin-isearch-loaders-load-chinese-sisheng ()
  "We don't use result, we need only loaded variables `sisheng-*'."
  (when (not (boundp 'sisheng-vowel-table))
    ;; for speed: `sisheng-regexp', `sisheng-vowel-table', `sisheng-syllable-table'.
    ;; (advice-add 'quail-make-sisheng-rules :override #'ignore)
    (pinyin-isearch-loaders--quail-extractor "chinese-sisheng")
    ;; (advice-remove 'quail-make-sisheng-rules #'ignore)
    ))

(provide 'pinyin-isearch-loaders)
;;; pinyin-isearch-loaders.el ends here
