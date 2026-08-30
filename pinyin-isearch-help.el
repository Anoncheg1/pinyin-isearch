;;; pinyin-isearch-help.el --- Exntended Help   -*- lexical-binding: t -*-

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
;; 1. Extended help map (inherits from isearch-help-map)
;; ============================================

(defun pinyin-isearch-help-describe-mode ()
  "Display documentation of Pinyin-Isearch mode."
  (interactive)
  (let ((display-buffer-overriding-action isearch--display-help-action))
    (describe-function #'pinyin-isearch-mode))
  (when isearch-mode (isearch-update)))

(defvar pinyin-isearch-help-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map isearch-help-map)
    (define-key map "p" #'pinyin-isearch-help-describe-mode)
    map)
  "Extended help map for `pinyin-isearch-mode'.
Inherits from `isearch-help-map' and add one key binding.")

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
   "  p   Show this pinyin-isearch help\n"
   "  q   Exit help")
  pinyin-isearch-help-map)

;; ============================================
;; 4. Help advice
;; ============================================

(defun pinyin-isearch-help-advice (orig-fun &rest args)
  "Show extended help when `pinyin-isearch-mode' is active.
Argument ORIG-FUN and ARGS is `isearch-help-for-help'."
  (if (and (boundp 'pinyin-isearch-mode) pinyin-isearch-mode)
      (let ((display-buffer-overriding-action isearch--display-help-action))
        (pinyin-isearch-help-for-help-internal)
        (isearch-update))
    (apply orig-fun args)))

;; ============================================
;; 4. Enable
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

;; ============================================
;; 5. Disable
;; ============================================

(defun pinyin-isearch-help-disable ()
  "Disable pinyin-isearch help extension."
  (interactive)
  (advice-remove 'isearch-help-for-help #'pinyin-isearch-help-advice))


(provide 'pinyin-isearch-help)

(provide 'pinyin-isearch-help)

;;; pinyin-isearch-help.el ends here
