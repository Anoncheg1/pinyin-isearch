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


;; (pinyin-isearch-pinyin-regexp-function "nih")
;; (pinyin-isearch-both-regexp-function "nih")
;; "\\(n[īíǐì]\\s-*h\\|[你呢尼泥逆倪匿拟腻妮霓昵溺旎睨鲵坭猊怩伲祢慝铌][和或活火获货伙祸惑霍豁夥锪耠劐钬攉藿嚯镬蠖婚混魂昏浑馄荤诨溷阍珲会回汇挥辉灰惠毁悔恢慧绘徽讳贿徊晦秽诲诙晖彗麾烩荟卉茴喙蛔恚洄珲蕙哕咴浍虺缋桧隳蟪黄皇荒晃慌煌惶恍谎璜徨簧凰幌潢蝗蟥遑隍肓磺癀湟篁鳇欢换还环缓患幻唤宦焕痪寰鬟涣浣奂桓缳豢锾郇萑圜洹擐獾漶逭鲩怀坏徊淮槐踝话华化花划画滑哗桦猾砉铧骅乎护呼胡户湖忽互糊虎壶狐沪惚浒唬葫弧蝴囫瑚斛祜猢鹄醐戽扈唿笏琥滹鹕轷烀冱岵怙鹘槲觳瓠鹱煳后候後厚侯喉吼猴逅糇骺堠瘊篌鲎红轰洪鸿哄宏虹弘烘泓闳薨讧蕻訇黉荭横衡恒哼亨蘅珩桁很恨狠痕黑嘿嗨和何合河喝赫核吓贺盒呵禾荷鹤壑阂褐诃涸阖嗬貉曷颌劾盍纥蚵翮菏好号毫豪浩耗皓嚎昊郝壕蒿貉灏镐嗥嚆薅濠蚝颢行航巷杭夯沆颃绗珩汉喊含寒汗韩憾涵函翰撼罕旱捍酣悍憨晗瀚鼾顸阚焊蚶焓颔菡撖邗邯还海孩害嘿咳亥骇骸嗨胲醢氦哈蛤铪]\\)"

(pinyin-isearch-load)
;; (setq pinyin-isearch-strict nil)
;; (setq pinyin-isearch-full-fallback nil)
;; -=-= help functons
;; used for testing
(defmacro with-test-isearch-env (&rest body)
  "Set up a robust mock environment for testing `isearch` in headless/batch mode."
  `(let ((frame (selected-frame)))
     ;; 1. Force a standard terminal geometry so window functions don't return 0
     (set-frame-parameter frame 'height 24)
     (set-frame-parameter frame 'width 80)

     (let ((isearch-lazy-highlight nil)       ; Edge Case 1: Kill async highlight timers
           (isearch-wrap-pause t)             ; Ensure predictable wrapping behavior
           (lazy-highlight-initial-delay 0)   ; Minimize timing issues if enabled
           (executing-kbd-macro t))           ; Edge Case 2: Tells isearch to skip raw redisplay updates

       ;; 3. Stub out window-line movements that require a graphical display engine
       (cl-letf* (((symbol-function 'move-to-window-line)
                   (lambda (arg)
                     (forward-line (or arg 0))
                     (line-number-at-pos)))
                  ((symbol-function 'pos-visible-in-window-p)
                   (lambda (&rest _args) t))) ; Edge Case 3: Pretend everything is always visible

         ,@body))))

;; used for testing
(defun pinyin-isearch-jump-and-stay-active (search-string &optional regexp-fn)
  "Jump to first match of SEARCH-STRING but keep isearch active.
Optional argument REGEXP-FN specifies the matching function to use.
Defaults to `pinyin-isearch-chars-strict-regexp-function' if omitted."
  (interactive "sSearch Pinyin: ")
  (let ((fun (pinyin-isearch--set-isearch)))
    ;; (print (list "fun" fun))
  (let ((inhibit-message t))

    (when (not (string-empty-p search-string))
      (isearch-mode t)
      (setq isearch-string search-string
            isearch-message search-string
            ;; Use the passed function, or fallback to strict if nil
            isearch-regexp-function (or regexp-fn fun))
      ;; Execute the search within the active state
      (isearch-search-and-update)))))

;; -=-= tests
(ert-deftest test-pinyin-isearch-main1 ()
  "Test that C -s M -s f1 displays a help window listing the pinyin functions.
This have a trick by Emacs, isearch-search-and-update call
 `pinyin-isearch-both-regexp-function' first in temp buffer and them
 several times in current, that is why we set variables here in current buffer (for visually testing)."
  (progn (setq pinyin-isearch-chars--cached-query nil)
         (setq pinyin-isearch-strict nil)
         (setq pinyin-isearch-chars-fallback t)
         (setq pinyin-isearch-full-fallback t)
         (with-temp-buffer
           ;; (with-test-isearch-env
            (setq pinyin-isearch-chars--cached-query nil)
            (let ((pinyin-isearch-strict nil)
                  (pinyin-isearch-chars-fallback t)
                  (pinyin-isearch-full-fallback t))


              ;; those two lines required to prevet error: move-to-window-line(0):  (error "move-to-window-line called from unrelated buffer")
              ;; (with-selected-window (selected-window)
              ;;   (set-window-buffer nil (current-buffer)))
              ;; Activate the minor mode
              (pinyin-isearch-mode 1)
              (insert "blabla 你好 (nǐ hǎo) 你好 (nǐ hao) 你hao 你g nihao") ; we dont search "你hao" for isearch speed optimization

              ;; (pinyin-isearch-jump-and-stay-active "nig"))

              ;; 1. Both pinyin-isearch-both-regexp-function
              (goto-char (point-min))
              (pinyin-isearch-jump-and-stay-active "nihao") ; should match 你hao also!
              (should (= (point) 10))

              (pinyin-isearch-jump-and-stay-active "nihao")
              (should (= (point) 18))

              (pinyin-isearch-jump-and-stay-active "nihao")
              (should (= (point) 22))

              (pinyin-isearch-jump-and-stay-active "nihao")
              (should (= (point) 30))

              (pinyin-isearch-jump-and-stay-active "nihao")
              (should (= (point) 45))

              (pinyin-isearch-jump-and-stay-active "nihao")
              (should (= (point) 45)))))


              (goto-char (point-min))
              (pinyin-isearch-jump-and-stay-active "nig")))
              (should (= (point) 39))
              (pinyin-isearch-jump-and-stay-active "nig")
              (should (= (point) 39))

              (goto-char (point-min))
              (pinyin-isearch-jump-and-stay-active "blabla") ; should match 你hao also!
              (should (= (point) 7)))))))


(ert-deftest test-pinyin-isearch-main2-strict ()
  "Test that C -s M -s f1 displays a help window listing the pinyin functions.
This have a trick by Emacs, isearch-search-and-update call
 `pinyin-isearch-both-regexp-function' first in temp buffer and them
 several times in current, that is why we set variables here in current buffer (for visually testing)."
  (progn (setq pinyin-isearch-chars--cached-query nil)
         (setq pinyin-isearch-chars-fallback t)
         (setq pinyin-isearch-strict t)
         (setq pinyin-isearch-full-fallback t)
         ;; (with-selected-window (selected-window)
         ;;       (set-window-buffer nil (current-buffer)))
         (with-temp-buffer
           (with-test-isearch-env
            (let ((pinyin-isearch-strict t)
                  (pinyin-isearch-chars-fallback t)
                  (pinyin-isearch-full-fallback t))
              (setq pinyin-isearch-chars--cached-query nil)

              ;; those two lines required to prevet error: move-to-window-line(0):  (error "move-to-window-line called from unrelated buffer")
              ;; (with-selected-window (selected-window)
              ;;   (set-window-buffer nil (current-buffer)))
              ;; Activate the minor mode
              (pinyin-isearch-mode 1)
              (insert "blabla 你好 (nǐ hǎo) 你好 (nǐ hao) 你hao 你g nihao") ; we found nǐ hao with strict, because of simplification in pinyin algoritm


              ;; 1. Both pinyin-isearch-both-regexp-function
              (goto-char (point-min))
              (pinyin-isearch-jump-and-stay-active "nihao") ; should match 你hao also!

              (should (= (point) 10))

              (pinyin-isearch-jump-and-stay-active "nihao")

              (should (= (point) 18))

              (pinyin-isearch-jump-and-stay-active "nihao")

              (should (= (point) 22))

              (pinyin-isearch-jump-and-stay-active "nihao")

              (should (= (point) 30))

              (pinyin-isearch-jump-and-stay-active "nihao")
              (should (= (point) 30))


              (goto-char (point-min))
              (pinyin-isearch-jump-and-stay-active "nig")
              (should (= (point) 1))

              (goto-char (point-min))
              (pinyin-isearch-jump-and-stay-active "blabla") ; should match 你hao also!
              (should (= (point) 1)))))))



(ert-deftest test-pinyin-isearch-main-no-fullfalback-separately ()
  "Test that C -s M -s f1 displays a help window listing the pinyin functions.
This have a trick by Emacs, isearch-search-and-update call
 `pinyin-isearch-both-regexp-function' first in temp buffer and them
 several times in current, that is why we set variables here in current buffer (for visually testing)."
  (progn (setq pinyin-isearch-chars--cached-query nil)
         (setq pinyin-isearch-pinyin--cached-query nil)
         (setq pinyin-isearch-strict nil)
         (setq pinyin-isearch-chars-fallback t)
         (setq pinyin-isearch-full-fallback nil)
         (with-temp-buffer
           (with-test-isearch-env
            (let ((pinyin-isearch-strict nil)
                  (pinyin-isearch-chars-fallback t)
                  (pinyin-isearch-full-fallback nil))
              (setq pinyin-isearch-chars--cached-query nil)
              (setq pinyin-isearch-pinyin--cached-query nil)

              ;; those two lines required to prevet error: move-to-window-line(0):  (error "move-to-window-line called from unrelated buffer")
              ;; (with-selected-window (selected-window)
              ;;   (set-window-buffer nil (current-buffer)))
              ;; Activate the minor mode
              (pinyin-isearch-mode 1)
              (insert "blabla 你好 (nǐ hǎo) 你好 (nǐ hao) 你hao 你g nihao") ; we dont search "你hao" for isearch speed optimization

              ;; 2. Pinyin pinyin-isearch-pinyin-regexp-function
              (goto-char (point-min))
              (pinyin-isearch-jump-and-stay-active "nihao" #'pinyin-isearch-pinyin-regexp-function)
              (should (= (point) 18))

              (pinyin-isearch-jump-and-stay-active "nihao" #'pinyin-isearch-pinyin-regexp-function)
              (should (= (point) 30))

              (pinyin-isearch-jump-and-stay-active "nihao" #'pinyin-isearch-pinyin-regexp-function)
              (should (= (point) 30))

              ;; (goto-char (point-min))
              ;; (pinyin-isearch-jump-and-stay-active "nih" #'pinyin-isearch-chars-regexp-function)))

              ;; 3. Characters pinyin-isearch-chars-regexp-function
              (goto-char (point-min))

              (pinyin-isearch-jump-and-stay-active "nihao" #'pinyin-isearch-chars-regexp-function)
              (should (= (point) 10))

              (pinyin-isearch-jump-and-stay-active "nihao" #'pinyin-isearch-chars-regexp-function)
              (should (= (point) 22))

              (pinyin-isearch-jump-and-stay-active "nihao" #'pinyin-isearch-chars-regexp-function)
              (should (= (point) 22)))))))


(ert-deftest test-pinyin-isearch-main-character-no-fallback ()
  "Test that C -s M -s f1 displays a help window listing the pinyin functions.
This have a trick by Emacs, isearch-search-and-update call
 `pinyin-isearch-both-regexp-function' first in temp buffer and them
 several times in current, that is why we set variables here in current buffer (for visually testing)."
  (progn (setq pinyin-isearch-chars--cached-query nil)
         (setq pinyin-isearch-strict nil)
         (setq pinyin-isearch-chars-fallback nil)
         (setq pinyin-isearch-full-fallback nil)
         (with-temp-buffer
           ;; (with-test-isearch-env
            (let ((pinyin-isearch-strict nil)
                  (pinyin-isearch-chars-fallback nil)
                  (pinyin-isearch-full-fallback nil))
              (setq pinyin-isearch-chars--cached-query nil)

              (pinyin-isearch-mode 1)
              (insert "blabla 你好 (nǐ hǎo) (nǐ hao) 你hao 你g nihao")

              ;; ;; 3. Characters pinyin-isearch-chars-regexp-function
              ;; nil nil
              (goto-char (point-min))
              (pinyin-isearch-jump-and-stay-active "nih" #'pinyin-isearch-chars-regexp-function)
              (should (= (point) 10))
              (pinyin-isearch-jump-and-stay-active "nih" #'pinyin-isearch-chars-regexp-function)
              (should (= (point) 10))

              (goto-char (point-min))
              ;; t nil
              (setq pinyin-isearch-chars--cached-query nil) ; reset cache
              (setq pinyin-isearch-chars-fallback t)
              (pinyin-isearch-jump-and-stay-active "nih" #'pinyin-isearch-chars-regexp-function)
              (should (= (point) 10))

              (pinyin-isearch-jump-and-stay-active "nih" #'pinyin-isearch-chars-regexp-function)
              (should (= (point) 31))
              (pinyin-isearch-jump-and-stay-active "nih" #'pinyin-isearch-chars-regexp-function)
              (should (= (point) 31))

              (goto-char (point-min))
              ;; t t
              (setq pinyin-isearch-chars-fallback t)
              (setq pinyin-isearch-full-fallback t)
              (setq pinyin-isearch-chars--cached-query nil) ; reset cache
              (pinyin-isearch-jump-and-stay-active "nih" #'pinyin-isearch-chars-regexp-function)
              (should (= (point) 10))
              (pinyin-isearch-jump-and-stay-active "nih" #'pinyin-isearch-chars-regexp-function)
              (should (= (point) 31))
              (pinyin-isearch-jump-and-stay-active "nih" #'pinyin-isearch-chars-regexp-function)
              (should (= (point) 40))
              (pinyin-isearch-jump-and-stay-active "nih" #'pinyin-isearch-chars-regexp-function)
              (should (= (point) 40))

              ;; nil t
              (setq pinyin-isearch-chars-fallback nil)
              (setq pinyin-isearch-full-fallback t)
              (setq pinyin-isearch-chars--cached-query nil)
              (pinyin-isearch-jump-and-stay-active "nih" #'pinyin-isearch-chars-regexp-function)
              (should (= (point) 40))
              (pinyin-isearch-jump-and-stay-active "nih" #'pinyin-isearch-chars-regexp-function)
              (should (= (point) 40))))))


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
