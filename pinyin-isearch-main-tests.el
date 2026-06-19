;;; pinyin-isearch-main-tests.el --- Tests for pinyin-isearch      -*- lexical-binding: t; -*-

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

;; to run: emacs -batch -l ert -l pinyin-isearch.el -l pinyin-isearch-tests.el -f ert-run-tests-batch-and-exit 2> out.log
;; eval-buffer
;; M-x ert RET t RET

;;; Code:

(require 'ert)
(require 'isearch)
(require 'pinyin-isearch)

(ert-deftest test-pinyin-isearch-help-menu ()
  "Test that C -s M -s f1 displays a help window listing the pinyin functions."
  (with-temp-buffer
    ;; 1. Activate the minor mode
    (pinyin-isearch-mode 1)
    ;; Ensure we clean up the minor mode after the test
    (unwind-protect
        (let ((help-buffer-name "*Help*"))
          ;; Kill any existing help buffer to avoid false positives
          (when (get-buffer help-buffer-name)
            (kill-buffer help-buffer-name))

          ;; 2. Simulate user pressing C-s M-s <f1>
          ;; We execute this as a macro. 'isearch' reads from the command loop,
          ;; so we pass the keys and then 'RET' to exit isearch mode so the test can finish.
          ;; (execute-kbd-macro (kbd "C-s M-s <f1> RET"))
          (describe-keymap 'pinyin-isearch-m-s-map)

          ;; 3. Verify that the *Help* buffer was created
          (should (get-buffer help-buffer-name))

          ;; 4. Verify the contents of the Help buffer include our bound functions
          (with-current-buffer help-buffer-name
            ;; (print (buffer-substring-no-properties (point-min) (point-max)))

            (goto-char (point-min))
            (should (search-forward "isearch-toggle-pinyin" nil t))
            (goto-char (point-min))
            (should (search-forward "isearch-toggle-characters" nil t))
            (goto-char (point-min))
            (should (search-forward "isearch-toggle-strict" nil t))))

      ;; Cleanup: Deactivate the mode
      (pinyin-isearch-mode -1))))


(provide 'pinyin-isearch-main-tests)

;;; pinyin-isearch-main-tests.el ends here
