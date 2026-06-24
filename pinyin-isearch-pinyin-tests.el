;;; pinyin-isearch-pinyin-tests.el --- Tests for pinyin-isearch pinyin mode   -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc

;; Author: Anoncheg1
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; to run: emacs -batch -l ert -l pinyin-isearch.el -l pinyin-isearch-pinyin-tests.el -f ert-run-tests-batch-and-exit 2> out.log
;; eval-buffer
;; M-x ert RET t RET

;;; Code:

(require 'ert)
(require 'pinyin-isearch-pinyin)

(defvar pinyin-isearch-strict nil) ; as in pinyin-isearch.el
(defvar pinyin-isearch-full-fallback t) ; as in pinyin-isearch.el


(pinyin-isearch-pinyin-load) ; activate pinyin-isearch-pinyin

(ert-deftest test-pinyin-isearch-pinyin--get_vowel_from_sisheng ()
  (with-temp-buffer
    (should (equal (pinyin-isearch-pinyin--get_vowel_from_sisheng "zuō") "o"))
    (should (equal (pinyin-isearch-pinyin--get_vowel_from_sisheng "nüē") "ue"))
    )
)


(ert-deftest test-pinyin-isearch-pinyin--vowels-to-regex ()
  (with-temp-buffer
    (setq-local pinyin-isearch-full-fallback nil)
    (should (equal (pinyin-isearch-pinyin--vowels-to-regex '("u" "o")) "\\([ūúǔùǖǘǚǜ]\\s-*o\\|u[ōóǒò]\\)"))
    (setq-local pinyin-isearch-full-fallback t)
    (should (equal (pinyin-isearch-pinyin--vowels-to-regex '("u")) "[uūúǔùǖǘǚǜ]"))
    (setq-local pinyin-isearch-full-fallback nil)
    (should (equal (pinyin-isearch-pinyin--vowels-to-regex '("u")) "[ūúǔùǖǘǚǜ]"))
    (should (equal (pinyin-isearch-pinyin--vowels-to-regex '("u" "ue")) "\\([ūúǔùǖǘǚǜ]\\s-*e\\|ü[ēéěè]\\)"))
    )
)


(ert-deftest test-pinyin--get-position-first-syllable ()
  (with-temp-buffer
    (should (equal (pinyin-isearch-pinyin--get-position-first-syllable "zuom") '(3 "u" "o")))
    (should (equal (pinyin-isearch-pinyin--get-position-first-syllable "svssvv") '(nil)))
    (should (equal (pinyin-isearch-pinyin--get-position-first-syllable "zux") '(2 "u")))
    (should (equal (pinyin-isearch-pinyin--get-position-first-syllable "zu") '(2 "u")))
    (should (equal (pinyin-isearch-pinyin--get-position-first-syllable "nue") '(3 "u" "ue")))
    (should (equal (pinyin-isearch-pinyin--get-position-first-syllable "pin") '(2 "i")))
    (should (equal (pinyin-isearch-pinyin--get-position-first-syllable "jiaoshenme") '(3 "i" "a")))
    )
)

(ert-deftest test-pinyin-isearch-pinyin--make-syllable-to-regex ()
  (with-temp-buffer
    (setq-local pinyin-isearch-full-fallback nil)
    (should (equal (pinyin-isearch-pinyin--make-syllable-to-regex "zuo" '(3 "u" "o")) "z\\([ūúǔùǖǘǚǜ]\\s-*o\\|u[ōóǒò]\\)"))
    (setq-local pinyin-isearch-full-fallback t)
    (should (equal (pinyin-isearch-pinyin--make-syllable-to-regex "zuo" '(3 "u" "o")) "z\\([uūúǔùǖǘǚǜ]\\s-*o\\|u[oōóǒò]\\)"))
    (setq-local pinyin-isearch-full-fallback nil)
    (should (equal (pinyin-isearch-pinyin--make-syllable-to-regex "zu" '(2 "u")) "z[ūúǔùǖǘǚǜ]"))
    (should (equal (pinyin-isearch-pinyin--make-syllable-to-regex "nue" '(3 "u" "ue")) "n\\([ūúǔùǖǘǚǜ]\\s-*e\\|ü[ēéěè]\\)"))
    (should (equal (pinyin-isearch-pinyin--make-syllable-to-regex "nue" '(nil)) "nue"))
    )
)


(ert-deftest test-pinyin-brute-replace ()
  (with-temp-buffer
    (should (equal (pinyin-isearch-pinyin--brute-replace "zuss") "z\\s-*[ūúǔùǖǘǚǜ]\\s-*s\\s-*s"))
    (should (equal (pinyin-isearch-pinyin--brute-replace "zuss" :normal t) "z\\s-*[uūúǔùǖǘǚǜ]\\s-*s\\s-*s"))
    (should (equal (pinyin-isearch-pinyin--brute-replace "zenme") "z\\s-*[ēéěè]\\s-*n\\s-*m\\s-*[ēéěè]"))
    (should (equal (pinyin-isearch-pinyin--brute-replace "zenme" :normal t) "z\\s-*[eēéěè]\\s-*n\\s-*m\\s-*[eēéěè]"))
    (should (equal (pinyin-isearch-pinyin--brute-replace "oshenme" :normal t) "[oōóǒò]\\s-*s\\s-*h\\s-*[eēéěè]\\s-*n\\s-*m\\s-*[eēéěè]"))
    )
)

(ert-deftest test-pinyin-isearch-pinyin-regexp-function ()
  (with-temp-buffer
    ;; (pinyin-isearch-mode)
    (setq-local pinyin-isearch-full-fallback nil)
    (should (equal (pinyin-isearch-pinyin-regexp-function "") ""))
    (should (equal (pinyin-isearch-pinyin-regexp-function "n") "n"))
    (should (equal (pinyin-isearch-pinyin-regexp-function "ssd") "ssd"))
    (should (equal (pinyin-isearch-pinyin-regexp-function "nu") "n[ūúǔùǖǘǚǜ]"))
    (should (equal (pinyin-isearch-pinyin-regexp-function "me") "m[ēéěè]"))
    (setq-local pinyin-isearch-full-fallback t)
    (should (equal (pinyin-isearch-pinyin-regexp-function "nu") "n[uūúǔùǖǘǚǜ]"))
    (should (equal (pinyin-isearch-pinyin-regexp-function "me") "m[eēéěè]"))

    (should (equal (pinyin-isearch-pinyin-regexp-function "zuo") "z\\([uūúǔùǖǘǚǜ]\\s-*o\\|u[oōóǒò]\\)"))
    (setq-local pinyin-isearch-full-fallback nil)
    (setq-local pinyin-isearch-pinyin--cached-query nil) ; reset cache
    (should (equal (pinyin-isearch-pinyin-regexp-function "zuo") "z\\([ūúǔùǖǘǚǜ]\\s-*o\\|u[ōóǒò]\\)"))

    (should (equal (pinyin-isearch-pinyin-regexp-function "zuozuo") "z\\([ūúǔùǖǘǚǜ]\\s-*o\\|u[ōóǒò]\\)\\s-*z\\s-*[uūúǔùǖǘǚǜ]\\s-*[oōóǒò]"))
    (should (equal (pinyin-isearch-pinyin-regexp-function "zuo me") "z\\([ūúǔùǖǘǚǜ]\\s-*o\\|u[ōóǒò]\\)\\s-* \\s-*m\\s-*[eēéěè]"))
    (should (equal (pinyin-isearch-pinyin-regexp-function "zuome") "z\\([ūúǔùǖǘǚǜ]\\s-*o\\|u[ōóǒò]\\)\\s-*m\\s-*[eēéěè]"))
    (should (equal (pinyin-isearch-pinyin-regexp-function "zuom") "z\\([ūúǔùǖǘǚǜ]\\s-*o\\|u[ōóǒò]\\)\\s-*m"))
    (should (equal (pinyin-isearch-pinyin-regexp-function "zuomezuome") "z\\([ūúǔùǖǘǚǜ]\\s-*o\\|u[ōóǒò]\\)\\s-*m\\s-*[eēéěè]\\s-*z\\s-*[uūúǔùǖǘǚǜ]\\s-*[oōóǒò]\\s-*m\\s-*[eēéěè]"))
    (should (equal (pinyin-isearch-pinyin-regexp-function "nuan") "n\\([ūúǔùǖǘǚǜ]\\s-*a\\|u[āáǎà]\\)n"))
    (should (equal (pinyin-isearch-pinyin-regexp-function "nue") "n\\([ūúǔùǖǘǚǜ]\\s-*e\\|ü[ēéěè]\\)"))
    (should (equal (pinyin-isearch-pinyin-regexp-function "pin") "p[īíǐì]\\s-*n"))
    (should (equal (pinyin-isearch-pinyin-regexp-function "jiaoshenme") "j\\([īíǐì]\\s-*a\\|i[āáǎà]\\)\\s-*[oōóǒò]\\s-*s\\s-*h\\s-*[eēéěè]\\s-*n\\s-*m\\s-*[eēéěè]"))
    (setq pinyin-isearch-strict nil) ; default
    (should (equal (pinyin-isearch-pinyin-regexp-function "hi") "hi"))
    (setq pinyin-isearch-strict t)
    (should (equal (pinyin-isearch-pinyin-regexp-function "hi") nil))
    )
)


;; (ert-deftest pinyin--sisheng-to-normal ()
;;   (with-temp-buffer
;;     (pinyin-isearch-mode)
;;     (should (equal (pinyin-isearch-pinyin--sisheng-to-normal "nüē") "nue"))
;;     )
;; )
;; test
;; (print (pinyin-isearch-pinyin--get-position-first-syllable "bian")) ; bi or an or bian? "bī" "ān" "biān" ;; "b\\([īíǐì][āáǎà]\\|i[āáǎà]\\|[īíǐì]a\\)n"
;; (print (pinyin-isearch-pinyin--get-position-first-syllable "bian")) ; bi or bian? "bī" "biān" ;; "b\\(i[āáǎà]\\|[īíǐì]a\\)n"
;; "b([īíǐì][āáǎà]|i[āáǎà]|[īíǐì]a)n"
;; "b\\([īíǐì][āáǎà]\\|i[āáǎà]\\|[īíǐì]a\\)n"
;; (ert-run-tests-interactively "pinyin--prepare-query")
;; (ert-run-tests-interactively "pinyin--sisheng-to-normal")
;; (ert-run-tests-interactively "pinyin--prepare-query2")

(provide 'pinyin-isearch-pinyin-tests)
;;; pinyin-isearch-pinyin-tests.el ends here
