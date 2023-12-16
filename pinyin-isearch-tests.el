;;; csv-mode-tests.el --- Tests for CSV mode         -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Free Software Foundation, Inc

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

;; to run: emacs -batch -l ert -l pinyin-isearch.el -l pinyin-isearch-tests.el -f ert-run-tests-batch-and-exit 2> out.log
;; eval-buffer
;; M-x ert

;;; Code:

(require 'ert)
(require 'pinyin-isearch)

;; (with-my-fixture)
;; pinyin-isearch-mode
(ert-deftest pinyin--make-sisheng-to-regex ()
  (with-temp-buffer
    (pinyin-isearch-mode)
    (should (equal (pinyin--make-sisheng-to-regex "zhuō") "zhu[ōóǒò]"))
    (should (equal (pinyin--make-sisheng-to-regex "nüē") "nü[ēéěè]"))
    )
)

(ert-deftest pinyin--get-position-first-syllable ()
  (with-temp-buffer
    (pinyin-isearch-mode)
    (should (eq (pinyin--get-position-first-syllable "zuom") 3))
    (should (eq (pinyin--get-position-first-syllable "svssvv") nil))
    (should (eq (pinyin--get-position-first-syllable "zu") 2))
    )
)

(ert-deftest pinyin-brute-replace ()
  (with-temp-buffer
    (pinyin-isearch-mode)
    (should (equal (pinyin-brute-replace "zuss") "z[ūúǔùǖǘǚǜ]\\s-*s\\s-*s"))
    (should (equal (pinyin-brute-replace "zenme") "z[ēéěè]\\s-*n\\s-*m\\s-*[ēéěè]"))
    ;; not required now: (should (equal (pinyin-brute-replace "zenme" t) "z[eēéěè]\\s-*n\\s-*m\\s-*[eēéěè]"))
    )
)

(ert-deftest pinyin--prepare-query1 ()
  (with-temp-buffer
    (pinyin-isearch-mode)
    (should (equal (pinyin--prepare-query "") ""))
    (should (equal (pinyin--prepare-query "n") "n"))
    (should (equal (pinyin--prepare-query "nu") "n[ūúǔùǖǘǚǜ]"))
    )
)

(ert-deftest pinyin--prepare-query2 ()
  (with-temp-buffer
    (pinyin-isearch-mode)
    (should (equal (pinyin--prepare-query "ssd") "ssd"))
    (should (equal (pinyin--prepare-query "zuo") "zu[ōóǒò]"))
    (should (equal (pinyin--prepare-query "me") "m[ēéěè]"))
    (should (equal (pinyin--prepare-query "zuozuo") "zu[ōóǒò]\\s-*z[uūúǔùǖǘǚǜ]\\s-*[oōóǒò]"))
    (should (equal (pinyin--prepare-query "zuo me") "zu[ōóǒò]\\s-* m\\s-*[eēéěè]"))
    (should (equal (pinyin--prepare-query "zuome") "zu[ōóǒò]\\s-*m[eēéěè]"))
    (should (equal (pinyin--prepare-query "zuom") "zu[ōóǒò]\\s-*m"))
    (should (equal (pinyin--prepare-query "zuomezuome") "zu[ōóǒò]\\s-*m[eēéěè]\\s-*z\\s-*[uūúǔùǖǘǚǜ]\\s-*[oōóǒò]\\s-*m\\s-*[eēéěè]"))
    (should (equal (pinyin--prepare-query "nuan") "nu[āáǎà]n"))
    (should (equal (pinyin--prepare-query "nue") "nü[ēéěè]"))
    )
)


;; (ert-deftest pinyin--sisheng-to-normal ()
;;   (with-temp-buffer
;;     (pinyin-isearch-mode)
;;     (should (equal (pinyin--sisheng-to-normal "nüē") "nue"))
;;     )
;; )
;; test
;;  ; TODO replace v with u

;; (ert-run-tests-interactively "pinyin--prepare-query")
;; (ert-run-tests-interactively "pinyin--sisheng-to-normal")
;; (ert-run-tests-interactively "pinyin--prepare-query2")

(provide 'pinyin-isearch-tests)
;;; pinyin-isearch-tests.el ends here
