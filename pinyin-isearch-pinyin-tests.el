;;; pinyin-isearch-pinyin-tests.el --- Tests for pinyin-isearch pinyin mode   -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc

;; Author: Anoncheg1
;; Keywords:

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

;; to run: emacs -batch -l ert -l pinyin-isearch.el -l pinyin-isearch-pinyin-tests.el -f ert-run-tests-batch-and-exit 2> out.log
;; eval-buffer
;; M-x ert RET t RET

;;; Code:

(require 'ert)
(require 'pinyin-isearch-pinyin)

(defvar pinyin-isearch-strict nil) ; as in pinyin-isearch.el
(defvar pinyin-isearch-full-fallback t) ; as in pinyin-isearch.el


(pinyin-isearch-pinyin-load) ; activate pinyin-isearch-pinyin

(ert-deftest test-pinyin-isearch-pinyin--get-vowel-from-sisheng ()
  (with-temp-buffer
    (should (equal (pinyin-isearch-pinyin--get-vowel-from-sisheng "zuō") "o"))
    (should (equal (pinyin-isearch-pinyin--get-vowel-from-sisheng "nüē") "ue"))
    )
)


(ert-deftest test-pinyin-isearch-pinyin--vowels-to-regex ()
  (with-temp-buffer
    (setq-local pinyin-isearch-full-fallback nil)
    (should (equal (pinyin-isearch-pinyin--vowels-to-regex '("u" "o")) "\\([ūúǔùǖǘǚǜ]['’]?[ōóǒò]\\|u[ōóǒò]\\)"))
    (setq-local pinyin-isearch-full-fallback t)
    (should (equal (pinyin-isearch-pinyin--vowels-to-regex '("u")) "[uūúǔùǖǘǚǜ]"))
    (setq-local pinyin-isearch-full-fallback nil)
    (should (equal (pinyin-isearch-pinyin--vowels-to-regex '("u")) "[ūúǔùǖǘǚǜ]"))
    (should (equal (pinyin-isearch-pinyin--vowels-to-regex '("u" "e")) "\\([ūúǔùǖǘǚǜ]['’]?[ēéěè]\\|u[ēéěè]\\)"))
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
    (should (equal (pinyin-isearch-pinyin--make-syllable-to-regex "zuo" '("u" "o")) "z\\([ūúǔùǖǘǚǜ]['’]?[ōóǒò]\\|u[ōóǒò]\\)"))
    (setq-local pinyin-isearch-full-fallback t)
    (should (equal (pinyin-isearch-pinyin--make-syllable-to-regex "zuo" '("u" "o")) "z\\([uūúǔùǖǘǚǜ]['’]?[oōóǒò]\\|u[oōóǒò]\\)"))
    (setq-local pinyin-isearch-full-fallback nil)
    (should (equal (pinyin-isearch-pinyin--make-syllable-to-regex "zu" '("u")) "z[ūúǔùǖǘǚǜ]"))
    (should (equal (pinyin-isearch-pinyin--make-syllable-to-regex "nue" '("u" "ue")) "n\\([ūúǔùǖǘǚǜ]['’]?[ēéěè]\\|u[ēéěè]\\)"))
    (should (equal (pinyin-isearch-pinyin--make-syllable-to-regex "nue" nil) "nue"))
    (setq-local pinyin-isearch-full-fallback t)
    (should (equal (pinyin-isearch-pinyin--make-syllable-to-regex "nue" '("u" "ue")) "n\\([uūúǔùǖǘǚǜ]['’]?[eēéěè]\\|u[eēéěè]\\)"))
    (should (equal (pinyin-isearch-pinyin--make-syllable-to-regex "nue" nil) "nue"))
    )
)


(ert-deftest test-pinyin-brute-replace ()
  (with-temp-buffer
    (should (equal (pinyin-isearch-pinyin--brute-replace "zuss") "z\\s-*[ūúǔùǖǘǚǜ]\\s-*s\\s-*s"))
    (should (equal (pinyin-isearch-pinyin--brute-replace "zuss" t) "z\\s-*[uūúǔùǖǘǚǜ]\\s-*s\\s-*s"))
    (should (equal (pinyin-isearch-pinyin--brute-replace "zenme") "z\\s-*['’]?[ēéěè]\\s-*n\\s-*m\\s-*['’]?[ēéěè]"))
    (should (equal (pinyin-isearch-pinyin--brute-replace "zenme" t) "z\\s-*['’]?[eēéěè]\\s-*n\\s-*m\\s-*['’]?[eēéěè]"))
    (should (equal (pinyin-isearch-pinyin--brute-replace "oshenme" t) "['’]?[oōóǒò]\\s-*s\\s-*h\\s-*['’]?[eēéěè]\\s-*n\\s-*m\\s-*['’]?[eēéěè]"))
    )
)

(ert-deftest test-pinyin-isearch-pinyin-regexp-function ()
  (let ((pinyin-isearch-full-fallback nil)
        (pinyin-isearch-strict nil))
    (should (equal (pinyin-isearch-pinyin-regexp-function "") regexp-unmatchable))
    (should (equal (pinyin-isearch-pinyin-regexp-function "n") regexp-unmatchable))
    (should (equal (pinyin-isearch-pinyin-regexp-function "ssd") regexp-unmatchable))
    (should (equal (pinyin-isearch-pinyin-regexp-function "nu") "n[ūúǔùǖǘǚǜ]"))
    (should (equal (pinyin-isearch-pinyin-regexp-function "me") "m[ēéěè]"))
    (should (equal (pinyin-isearch-pinyin-regexp-function "bla") regexp-unmatchable))
    (should (equal (pinyin-isearch-pinyin-regexp-function "zuo") "z\\([ūúǔùǖǘǚǜ]['’]?[ōóǒò]\\|u[ōóǒò]\\)"))
    (should (equal (pinyin-isearch-pinyin-regexp-function "hi") regexp-unmatchable))
    )
  (let ((pinyin-isearch-full-fallback t)
        (pinyin-isearch-strict nil))
    (should (equal (pinyin-isearch-pinyin-regexp-function "") regexp-unmatchable))
    (should (equal (pinyin-isearch-pinyin-regexp-function "ssd") "ssd"))
    (should (equal (pinyin-isearch-pinyin-regexp-function "nu") "n[uūúǔùǖǘǚǜ]"))
    (should (equal (pinyin-isearch-pinyin-regexp-function "me") "m[eēéěè]"))
    (should (equal (pinyin-isearch-pinyin-regexp-function "zuo") "z\\([uūúǔùǖǘǚǜ]['’]?[oōóǒò]\\|u[oōóǒò]\\)"))
    )
    (let ((pinyin-isearch-full-fallback nil)
        (pinyin-isearch-strict t))
      (should (equal (pinyin-isearch-pinyin-regexp-function "zuo") "z\\([ūúǔùǖǘǚǜ]['’]?[ōóǒò]\\|u[ōóǒò]\\)"))
      (should (equal (pinyin-isearch-pinyin-regexp-function "zuozuo") "z\\([ūúǔùǖǘǚǜ]['’]?[ōóǒò]\\|u[ōóǒò]\\)\\s-*z\\s-*[uūúǔùǖǘǚǜ]\\s-*['’]?[oōóǒò]"))
      (should (equal (pinyin-isearch-pinyin-regexp-function "zuo me") "z\\([ūúǔùǖǘǚǜ]['’]?[ōóǒò]\\|u[ōóǒò]\\)\\s-* \\s-*m\\s-*['’]?[eēéěè]"))
      (should (equal (pinyin-isearch-pinyin-regexp-function "zuome") "z\\([ūúǔùǖǘǚǜ]['’]?[ōóǒò]\\|u[ōóǒò]\\)\\s-*m\\s-*['’]?[eēéěè]"))
      (should (equal (pinyin-isearch-pinyin-regexp-function "zuom") "z\\([ūúǔùǖǘǚǜ]['’]?[ōóǒò]\\|u[ōóǒò]\\)\\s-*m"))
      (should (equal (pinyin-isearch-pinyin-regexp-function "zuomezuome") "z\\([ūúǔùǖǘǚǜ]['’]?[ōóǒò]\\|u[ōóǒò]\\)\\s-*m\\s-*['’]?[eēéěè]\\s-*z\\s-*[uūúǔùǖǘǚǜ]\\s-*['’]?[oōóǒò]\\s-*m\\s-*['’]?[eēéěè]"))
      (should (equal (pinyin-isearch-pinyin-regexp-function "nuan") "n\\([ūúǔùǖǘǚǜ]['’]?[āáǎà]\\|u[āáǎà]\\)n"))
      (should (equal (pinyin-isearch-pinyin-regexp-function "nue") "n\\([ūúǔùǖǘǚǜ]['’]?[ēéěè]\\|u[ēéěè]\\)"))
      (should (equal (pinyin-isearch-pinyin-regexp-function "pin") "p[īíǐì]\\s-*n"))
      (should (equal (pinyin-isearch-pinyin-regexp-function "jiaoshenme") "j\\([īíǐì]['’]?[āáǎà]\\|i[āáǎà]\\)\\s-*['’]?[oōóǒò]\\s-*s\\s-*h\\s-*['’]?[eēéěè]\\s-*n\\s-*m\\s-*['’]?[eēéěè]"))
      (should (equal (pinyin-isearch-pinyin-regexp-function "hi") regexp-unmatchable))
      )

    (let ((pinyin-isearch-full-fallback t)
          (pinyin-isearch-strict t))
      (should (equal (pinyin-isearch-pinyin-regexp-function "hi") regexp-unmatchable))
      (should (equal (pinyin-isearch-pinyin-regexp-function "a") regexp-unmatchable))
    )
)


(ert-deftest test-pinyin-isearch--sisheng-to-normal ()
    (should (equal (pinyin-isearch-pinyin--sisheng-to-normal "nüē") "nue"))
    (should (equal (pinyin-isearch-pinyin--sisheng-to-normal "zhuō") "zhuo"))
    ;; (should (equal (pinyin-isearch-pinyin--sisheng-to-normal "lüè") "lue"))
    (should (equal (pinyin-isearch-pinyin--sisheng-to-normal "fā") "fa"))
    ;; (should (equal (pinyin-isearch-pinyin--sisheng-to-normal "dì") "di"))
    ;; (should (equal (pinyin-isearch-pinyin--sisheng-to-normal "gǒu") "gou"))
    (should (equal (pinyin-isearch-pinyin--sisheng-to-normal "zao") "zao"))
    (should (equal (pinyin-isearch-pinyin--sisheng-to-normal "lve") "lve"))
    ;; (should (equal (pinyin-isearch-pinyin--sisheng-to-normal "lǜ") "lu"))
    ;; (should (equal (pinyin-isearch-pinyin--sisheng-to-normal "lü") "lu"))
    ;; (should (equal (pinyin-isearch-pinyin--sisheng-to-normal "nǚ") "nu"))
    (should (equal (pinyin-isearch-pinyin--sisheng-to-normal "huā") "hua"))
    ;; (should (equal (pinyin-isearch-pinyin--sisheng-to-normal "guǒ") "guo"))
    )

;; test
(ert-deftest test-pinyin-isearch--get-position-first-syllable ()
  (should (equal (pinyin-isearch-pinyin--get-position-first-syllable "ngigni") '(nil)))
  (should (equal (pinyin-isearch-pinyin--get-position-first-syllable "niggni") '(2 "i")))
  (should (equal (pinyin-isearch-pinyin--get-position-first-syllable "bian") '(4 "i" "a")))
  )

(provide 'pinyin-isearch-pinyin-tests)
;;; pinyin-isearch-pinyin-tests.el ends here
