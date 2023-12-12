;;; pinyin-isearch.el --- isearch mode for Chinese pinyin search.  -*- lexical-binding: t -*-

;; Copyright (c) 2023 Anoncheg1

;; Author: Anoncheg1
;; Keywords: convenience
;; URL: https://github.com/Anoncheg1/pinyin-isearch
;; Keywords: isearch
;; Version: 0.2
;; Package-Requires: ((emacs "29.1"))

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
;;; Code:


(defun pinyin-isearch ()
  (if isearch-regexp
      ;; normal execution if it is regex search
      (funcall original-isearch-search-fun-function)
  ;; else
  (lambda (string &optional bound noerror count)
    (let* ((st (regexp-quote string))
           (st (string-replace "a" "[āáǎà]" st))
           (st (string-replace "e" "[ēéěè]" st))
           (st (string-replace "o" "[ōóǒò]" st))
           (st (string-replace "i" "[īíǐì]" st))
           (st (string-replace "u" "[ūúǔùǚ]" st))
           (regexp st))
    (funcall
     (if isearch-forward #'re-search-forward #'re-search-backward)
     regexp bound noerror count)))))


(define-minor-mode pinyin-isearch-mode
  "In isearch C-s with pinyin you will be able to find pīnyīn."
    :lighter " p-isearch" :global nil :group 'isearch :version "29.1"
    (defvar-local pinyin-isearch-message-prefix
        (concat (propertize "[pinyin]" 'face 'bold) " "))

    (defadvice isearch-message-prefix (after pinyin-isearch-message-prefix activate)
      (if (and pinyin-isearch-mode (not isearch-regexp))
          (setq ad-return-value
                (concat pinyin-isearch-message-prefix ad-return-value))
        ad-return-value))

    ;; when mode activated:
    (when pinyin-isearch-mode
      ;; save
      (defvar-local original-isearch-search-fun-function isearch-search-fun-function))
    ;; remap:
    (setq-local isearch-search-fun-function 'pinyin-isearch)
    ;; disable:
    (if (not pinyin-isearch-mode)
        (setq-local isearch-search-fun-function original-isearch-search-fun-function))
    )


(provide 'pinyin-isearch)
