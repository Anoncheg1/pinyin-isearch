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

;; used for testing
(defun pinyin-isearch-jump-and-stay-active (search-string &optional regexp-fn)
  "Jump to first match of SEARCH-STRING but keep isearch active.
Optional argument REGEXP-FN specifies the matching function to use.
Defaults to `pinyin-isearch-chars-strict-regexp-function' if omitted."
  (interactive "sSearch Pinyin: ")
  (let ((inhibit-message t))
    (when (not (string-empty-p search-string))
      (isearch-mode t)
      (setq isearch-string search-string
            isearch-message search-string
            ;; Use the passed function, or fallback to strict if nil
            isearch-regexp-function (or regexp-fn (pinyin-isearch--set-isearch)))
      ;; Execute the search within the active state
      (isearch-search-and-update))))


(ert-deftest test-pinyin-isearch-main ()
  "Test that C -s M -s f1 displays a help window listing the pinyin functions."
  (with-temp-buffer
    ;; those two lines required to prevet error: move-to-window-line(0):  (error "move-to-window-line called from unrelated buffer")
    (with-selected-window (selected-window)
      (set-window-buffer nil (current-buffer))
    ;; Activate the minor mode
    (pinyin-isearch-mode 1)
    (insert "blabla 你好 (nǐ hǎo) 你好 (nǐ hao) 你hao 你g")

    ;; (pinyin-isearch-jump-and-stay-active "nig"))

    ;; 1. Both pinyin-isearch-both-regexp-function
    (goto-char (point-min))
    (pinyin-isearch-jump-and-stay-active "nihao")
    (should (= (point) 10))

    (pinyin-isearch-jump-and-stay-active "nihao")
    (should (= (point) 18))


    ;; (pinyin-isearch-jump-and-stay-active "nihao")
    ;; (point))

    ;; 2. Pinyin pinyin-isearch-pinyin-regexp-function
    (goto-char (point-min))
    (pinyin-isearch-jump-and-stay-active "nihao" #'pinyin-isearch-pinyin-regexp-function)
    (should (= (point) 18))

    (pinyin-isearch-jump-and-stay-active "nihao" #'pinyin-isearch-pinyin-regexp-function)
    (should (= (point) 30))



    ;; 3. Characters pinyin-isearch-chars-regexp-function
    (goto-char (point-min))
    (pinyin-isearch-jump-and-stay-active "nihao" #'pinyin-isearch-chars-regexp-function)
    (should (= (point) 10))

    (pinyin-isearch-jump-and-stay-active "nihao" #'pinyin-isearch-chars-regexp-function)
    (should (= (point) 22))

    ;; ;; 4. Characters pinyin-isearch-chars-regexp-function
    ;; (beginning-of-buffer)
    ;; (pinyin-isearch-jump-and-stay-active "nihao" #'pinyin-isearch-chars-strict-regexp-function)
    ;; (point))
    ;; (should (= (point) 10))

    ;; (pinyin-isearch-jump-and-stay-active "nihao" #'pinyin-isearch-chars-strict-regexp-function)
    ;; (should (= (point) 22))

    )))




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
