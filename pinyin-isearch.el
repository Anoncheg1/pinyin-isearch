;;; pinyin-isearch.el --- isearch mode for Chinese pinyin search.  -*- lexical-binding: t -*-

;; Copyright (c) 2023 Anoncheg1

;; Author: Anoncheg1
;; Keywords: convenience, isearch
;; URL: https://github.com/Anoncheg1/pinyin-isearch
;; Version: 0.6
;; Package-Requires: ((emacs "29.1"))

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

;; This package modifies isearch mode to allow search pīnyīn with
;; pinyin (without tones).
;; To activate use:
;; (require 'pinyin-isearch)
;; (pinyin-isearch-mode t) ;; or -*- mode: pinyin-isearch; -*-
;; From "quail/sisheng.el" used sisheng-regexp, sisheng-vowel-table
;; and sisheng-syllable-table.
;;; Code:

(condition-case args-out-of-range
    (load "quail/sisheng.el")
  (error nil))

(defconst pinyin-vowel-table
  '(("a" "[āáǎà]")
    ("e" "[ēéěè]")
    ("i" "[īíǐì]")
    ("o" "[ōóǒò]")
    ("u" "[ūúǔùǖǘǚǜ]")
    ("v" "[ūúǔùǖǘǚǜ]")
    ("ue" "[ūúǔùǖǘǚǜ][ēéěè]")
    ("ve" "ü[ēéěè]")))

(defconst pinyin-vowel-table-normal
  '(("a" "[aāáǎà]")
    ("e" "[eēéěè]")
    ("i" "[iīíǐì]")
    ("o" "[oōóǒò]")
    ("u" "[uūúǔùǖǘǚǜ]")
    ("ue" "[uü][eēéěè]")))

(defconst pinyin-isearch-message-prefix
        (concat (propertize "[pinyin]" 'face 'bold) " ")
"used when pinyin-isearch-mode is activated only")

(defun pinyin--make-sisheng-to-regex (syllable)
  "Convert SYLLABLE \"zhuō\" to \"zhu[...]\".
Used to create final regex."
  (let ((key-value-list))
    (string-match sisheng-regexp syllable)
    (let* (
           (vowel-match (downcase (match-string 0 syllable)))
           (vowel-list
            (cdr (assoc-string vowel-match sisheng-vowel-table)))
           (input-vowel (car vowel-list))
           (regex (car (cdr (assoc-string input-vowel pinyin-vowel-table))))
           )
    ;; replace ō with [ōóǒò]
    (replace-match regex nil nil syllable))
))


(defun pinyin--get-position-first-syllable(string)
  "Get position of first syllable in query STRING."
  (let ((pos 0)
        (first-chars)
         (num 0)
         (len (length string)))
    (while (< num (1- (length string) ))
      (setq pos (- len num))
      ;; cut first chars
      (setq first-chars (substring string 0 pos))
      ;; find it in table
      (setq syl (cdr (assoc first-chars pinyin-syllable-table)))
      ;; break
      (setq num (if syl 999 (1+ num)))
      )
    (if syl pos
      nil) ; else nil
    ))


(defun pinyin-brute-replace (st &optional &key normal)
  "Replace every vowels in 'ST' with wide range regex.
if 'NORMAL' add normal to regex."
  (let* (
           ;; ignore white spaces if query is more than 2 characters
         (st (if (length> st 1)
                 ;; then
                 (concat (substring st 0 2)
                         (mapconcat (lambda (x) (concat "\\s-*" x))
                                    (nthcdr 2 (split-string st "" t))))
               ;; else
               st)))
    ;; ignore tones, based on lisp/leim/quail/sisheng.el
    (if normal
        (dolist ( c (split-string "aeiou" "" t))
          (let ((vowel-list-regex
                 (car (cdr (assoc-string c pinyin-vowel-table-normal))) ))
            (setq st (string-replace c vowel-list-regex st)))
          )
      ;; else
      (dolist ( c (split-string "aeiou" "" t))
          (let ((vowel-list-regex
                 (car (cdr (assoc-string c pinyin-vowel-table))) ))
            (setq st (string-replace c vowel-list-regex st)))
          )
      )
    st)
  )

(defun pinyin--prepare-query (string)
  "Main function to convert query 'STRING' to regex for isearch."
  (let* ((st (regexp-quote string))
         ;; save length
         (len (length st))
         ;; get first longest syllable
         (first-syllable-pos (if (length> st 1)
                                 (pinyin--get-position-first-syllable st)
                               ;; else
                               nil)
                             )
         )

    ;; accurate regex for first syllable and brute for other
    (if first-syllable-pos
        ;; cut first sullable
        (let* ((first-syllable (substring string 0 first-syllable-pos))

               (first-syllable
                       (cdr (assoc first-syllable pinyin-syllable-table)))

               (first-syllable (pinyin--make-sisheng-to-regex first-syllable))
               ;; if others is not null
               (others (if (< first-syllable-pos len)
                           ;; others
                       (concat "\\s-*" (pinyin-brute-replace
                                        (substring st first-syllable-pos len)
                                        :normal t))
                       ;; else
                       nil)
                       ))
          (concat first-syllable others))
         st)
    )
  )

(defun pinyin--sisheng-to-normal (syllable)
  "Convert \"zhuō\" 'SYLLABLE' to \"zhuo\". Used to create list from original."
  (let ((key-value-list))
    (string-match sisheng-regexp syllable)
    ;; fin vowel
    (setq vowel-match (downcase (match-string 0 syllable)))
    (setq vowel-list
          (cdr (assoc-string vowel-match sisheng-vowel-table)))
    (setq input-vowel (car vowel-list))
    (setq base-key (nth 0 vowel-list))
    ;; fix for sisheng, we don't need "v"
    (setq base-key (if (equal base-key "v") "u"
                     ;; else
                     (if (equal base-key "ve") "ue" base-key)))
    (replace-match base-key nil nil syllable)
))


(defun pinyin--isearch-search-fun-function ()
  "Replacement for `isearch-search-fun-function'.
It modifies search query string and call isearch with regex."
  (if isearch-regexp
      ;; normal execution if it is regex search
      (funcall original-isearch-search-fun-function)
  ;; else
  (lambda (string &optional bound noerror count)
    (let ((regexp (pinyin--prepare-query string)))
      (funcall
       (if isearch-forward #'re-search-forward #'re-search-backward)
       regexp bound noerror count))))
  )


(defconst pinyin-syllable-table
    (mapcar (lambda (arg)
              (cons (pinyin--sisheng-to-normal arg) arg)
              )
            sisheng-syllable-table) ;; sequence
    "initialize syllable's table ((\"zhuo\" . \"zhuō\")...)"
  )

;;;###autoload
(define-minor-mode pinyin-isearch-mode
  "Modifies 'isearch-forward', to allow with query \"pinyin\" to find \"pīnyīn\"."
    :lighter " p-isearch" :global nil :group 'isearch :version "29.1"
    (defvar-local original-isearch-search-fun-function isearch-search-fun-function)
    ;; remap
    (setq-local isearch-search-fun-function 'pinyin--isearch-search-fun-function)
    ;; disable
    (if (not pinyin-isearch-mode)
        (setq-local isearch-search-fun-function original-isearch-search-fun-function))
    )


;; It Adds prefix to isearch prompt.
(defadvice isearch-message-prefix (after pinyin-isearch-message-prefix activate)
  (if (and pinyin-isearch-mode (not isearch-regexp))
      (setq ad-return-value
            (concat pinyin-isearch-message-prefix ad-return-value))
    ad-return-value))



(defvar-local pinyin--original-isearch-search-fun-function nil
  "place to save 'isearch-search-fun-function'")

(defun pinyin--isearch-restore ()
  "Used for hook: 'isearch-mode-end-hook'"
  (setq-local isearch-search-fun-function original-isearch-search-fun-function))

(defun pinyin-isearch-forward (&rest arg) ;; (&optional regexp-p no-recursive-edit)
  "Pinyin veriant of 'isearch-forward', just like in 'pinyin-isearch-mode'."
  (interactive "P\np")
  ;; make isearch our's
  (setq-local pinyin--original-isearch-search-fun-function isearch-search-fun-function)
  (setq-local isearch-search-fun-function 'pinyin--isearch-search-fun-function)


  (if (interactive-p)
      (call-interactively 'isearch-forward)
    ;; else
    (apply 'isearch-forward arg)
    )
  (add-hook 'isearch-mode-end-hook #'pinyin--isearch-restore)
)


(defun pinyin-isearch-backward (&rest arg)
  "Pinyin veriant of 'isearch-backward', just like in 'pinyin-isearch-mode'."
  (interactive "P\np")
  ;; make isearch our's
  (setq-local pinyin--original-isearch-search-fun-function isearch-search-fun-function)
  (setq-local isearch-search-fun-function 'pinyin--isearch-search-fun-function)

  (if (interactive-p)
      (call-interactively 'isearch-backward)
    ;; else
    (apply 'isearch-backward arg)
    )

  (add-hook 'isearch-mode-end-hook #'pinyin--isearch-restore)
)



(provide 'pinyin-isearch)
;;; pinyin-isearch.el ends here
