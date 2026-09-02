;;; pinyin-isearch-help.el --- Exntended Help   -*- lexical-binding: t -*-

;; Copyright (c) 2026 Anoncheg1

;; Author: Anoncheg1
;; Keywords: chinese, isearch, matching, convenience
;; URL: https://github.com/Anoncheg1/pinyin-isearch
;; Version: 1.7.2
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

;; For C-s F1 F1

;; We show help screen with when isearch with pinyin-isearch is active.
;; We show original help of isearch and inherit keys.

;; Activated in `pinyin-isearch-load'.

(require 'pinyin-isearch)

(require 'isearch)
;;; Code:

(eval-when-compile (require 'help-macro))


;; ============================================
;; 1. Show all pinyin keys and documentation in help screen
;; ============================================

(make-help-screen pinyin-isearch-help-for-help-internal
  (purecopy "Show pinyin-isearch help with key bindings and current status.")
  ;; Help text - computed dynamically to show current search mode
  (concat
   "=== Pinyin-Isearch Help ===\n\n"
   "Key bindings under M-s prefix:\n"
   "  M-s n   Toggle Pinyin+characters search\n"
   "  M-s p   Toggle Pinyin-only search\n"
   "  M-s h   Toggle characters-only search\n"
   "  M-s s   Toggle strict Pinyin+characters search\n"
   "  M-s u   Toggle strict characters-only search\n"
   "  M-s r   Return to standard (non-Pinyin) search\n\n"
   "Current search mode: "
   (if (and (boundp 'isearch-regexp-function) isearch-regexp-function)
       (symbol-name isearch-regexp-function)
     "standard")
   "\n\nHelp options (press key):\n"
   "  b   Show standard Isearch key bindings\n"
   "  k   Show documentation for a specific key\n"
   "  m   Show Isearch mode documentation\n"
   "  q   Exit help")
  isearch-help-map)

;; ============================================
;; 2. Help advice
;; ============================================

(defun pinyin-isearch-help-advice (orig-fun &rest args)
  "Show extended help when `pinyin-isearch-mode' is active.
Argument ORIG-FUN and ARGS is `isearch-help-for-help'."
  (if (and (boundp 'pinyin-isearch-mode) pinyin-isearch-mode)
      (let ((display-buffer-overriding-action isearch--display-help-action))
        (pinyin-isearch-help-for-help-internal)
        (isearch-update))
    ;; else
    (apply orig-fun args)))

;; ============================================
;; 3. Enable
;; ============================================

(defun pinyin-isearch-help-enable ()
  "Enable pinyin-isearch help extension.
To the isearch help screen when `pinyin-isearch-mode' is active, that is
 checked in `pinyin-isearch-help-advice'."
  (interactive)
  (when (and (boundp 'isearch--display-help-action)
             (boundp 'isearch-help-map))
    (unless (advice-member-p #'pinyin-isearch-help-advice 'isearch-help-for-help)
      (advice-add 'isearch-help-for-help :around #'pinyin-isearch-help-advice))
    (message "Pinyin-isearch help enabled")
    t))


(provide 'pinyin-isearch-help)

;;; pinyin-isearch-help.el ends here
