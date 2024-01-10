;;; pinyin-isearch-loaders.el --- Loaders of pinyin and hierogliphs from guail.  -*- lexical-binding: t -*-

;; -------------- tools for loading ---------------
(defvar pinyin-isearch--rules nil "used in advice")

(defun pinyin-isearch--quail-define-rules-advice (&rest args)
  (setq pinyin-isearch--rules args))


(defun pinyin-isearch--quail-extractor (quail-file)
  (advice-add 'quail-define-rules :before #'pinyin-isearch--quail-define-rules-advice)
  (load-file (locate-file quail-file load-path))
  (advice-remove 'quail-define-rules #'pinyin-isearch--quail-define-rules-advice)
  ;; return
  pinyin-isearch--rules)


(defun pinyin-isearch--punct-quail-filter (rules)
  "Load rules for single letters of punctuations."
   (seq-filter (lambda (x) (= (length (car x)) 1)) rules))

(defun add_one_to_another(c1 c2)
  (apply 'append (list c1 c2)))


;; ---------- load quail/PY.el for chinese hierogliphs ---------
(defconst pinyin-isearch--py-rules
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
    rul)) ; rules in form: (("a" "阿啊呵腌嗄锕吖") ("ai" "爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹")...


;; ------------- load punct and concatenate: py + punct --------

(defconst pinyin-isearch--punct-rules
  (pinyin-isearch--punct-quail-filter
   (pinyin-isearch--quail-extractor "quail/Punct.el"))
  "Extracted and filtered Chinese punctuation.")

(defconst pinyin-isearch--py-punct-rules
  (add_one_to_another pinyin-isearch--py-rules pinyin-isearch--punct-rules)
  "Extracted quail/PY.el + quail/Punct.el - Chinese heieroglyphs and punctuation.")

(provide 'pinyin-isearch-loaders)
;;; pinyin-isearch-loaders.el ends here
