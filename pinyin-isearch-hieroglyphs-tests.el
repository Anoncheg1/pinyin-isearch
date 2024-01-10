;;; pinyin-isearch-hieroglyphs-tests.el --- Tests for pinyin-isearch hiearoglyphs mode      -*- lexical-binding: t; -*-

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
;; M-x ert RET t RET

;;; Code:

(require 'ert)
(require 'pinyin-isearch-hieroglyphs)

(declare-function pinyin-isearch-hieroglyphs-regexp-function "pinyin-isearch-hieroglyphs" (string &optional lax))

(declare-function pinyin-isearch--hieroglyphs-recursion "pinyin-isearch-hieroglyphs" (st))


(ert-deftest pinyin-isearch--rules-to-first-syllable-letters ()
  (with-temp-buffer
    (should (equal (pinyin-isearch--rules-to-first-syllable-letters '(("a" "阿啊呵腌嗄锕吖") ("ai" "爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹"))) '(("ai" ("ai")) ("a" ("ai" "a")))))
    )
)

(ert-deftest pinyin-isearch--get-syllables-by-prefix ()
  (with-temp-buffer
    (should (equal (pinyin-isearch--get-syllables-by-prefix "a") '("ao" "ang" "an" "ai" "a") ))
    (should (equal (pinyin-isearch--get-syllables-by-prefix ">") '(">") ))
    (should (equal (pinyin-isearch--get-syllables-by-prefix ".") '(".") ))
    (should (equal (pinyin-isearch--get-syllables-by-prefix "n") '("nuo" "nue" "nuan" "nu" "nou" "nong" "niu" "ning" "nin" "nie" "niao" "niang" "nian" "ni" "ng" "neng" "nen" "nei" "ne" "nao" "nang" "nan" "nai" "na" "n") ))
    )
)

(ert-deftest pinyin-isearch--pinyin-to-hieroglyphs ()
  (with-temp-buffer
    (should (equal (pinyin-isearch--pinyin-to-hieroglyphs "a") "阿啊呵腌嗄锕吖"))
    (should (equal (pinyin-isearch--pinyin-to-hieroglyphs "g") "g"))
    (should (equal (pinyin-isearch--pinyin-to-hieroglyphs ".") "．\\.。・¨…∵∴°⊙"))
    (should (equal (pinyin-isearch--pinyin-to-hieroglyphs "nv") "女钕恧衄")) ; for ppl who likes "v"
    (should (equal (pinyin-isearch--pinyin-to-hieroglyphs "nu") "女钕恧衄怒努奴弩驽胬孥"))
    )
)

(ert-deftest pinyin-isearch--hieroglyphs-recursion ()
  (with-temp-buffer
    (should (equal (pinyin-isearch--hieroglyphs-recursion "a>") '((("a") (">"))) ))
    (should (equal (pinyin-isearch--hieroglyphs-recursion "a>.") '((("a") (">") ("."))) ))
    (should (equal (pinyin-isearch--hieroglyphs-recursion "a&&") '((("a")("\34&&"))) ))
    (should (equal (pinyin-isearch--hieroglyphs-recursion "an.") '((("a") ("n") (".")) (("an") ("."))) ))
    (should (equal (pinyin-isearch--hieroglyphs-recursion "cccc") '((("\34cccc"))) ))
    (should (equal (pinyin-isearch--hieroglyphs-recursion "a") '((("ao" "ang" "an" "ai" "a"))) ))
    (should (equal (pinyin-isearch--hieroglyphs-recursion "an") '((("a") ("nuo" "nue" "nuan" "nu" "nou" "nong" "niu" "ning" "nin" "nie" "niao" "niang" "nian" "ni" "ng" "neng" "nen" "nei" "ne" "nao" "nang" "nan" "nai" "na" "n")) (("ang" "an"))) ))
    (should (equal (pinyin-isearch--hieroglyphs-recursion "zuana") '((("zu") ("a") ("n") ("ao" "ang" "an" "ai" "a")) (("zu") ("a") ("nao" "nang" "nan" "nai" "na")) (("zu") ("an") ("ao" "ang" "an" "ai" "a")) (("zuan") ("ao" "ang" "an" "ai" "a"))) ))
    (should (equal (pinyin-isearch--hieroglyphs-recursion "zuhna") '((("zu") ("\34hna"))) ))
    (should (equal (pinyin-isearch--hieroglyphs-recursion "zu.na") '((("zu") (".") ("n") ("ao" "ang" "an" "ai" "a")) (("zu") (".") ("nao" "nang" "nan" "nai" "na"))) ))
    (should (equal (pinyin-isearch--hieroglyphs-recursion "nai") '((("n") ("a") ("\34i")) (("n") ("ai")) (("na") ("\34i")) (("nai"))) ))
    (setq pinyin-isearch-strict t)
    (should (equal (pinyin-isearch--hieroglyphs-recursion "a&&") nil ))
    (should (equal (pinyin-isearch--hieroglyphs-recursion "cccc") nil ))
    (should (equal (pinyin-isearch--hieroglyphs-recursion "zuhna") nil ))
    (should (equal (pinyin-isearch--hieroglyphs-recursion "nai") '((("n") ("ai")) (("nai"))) ))
    (should (equal (pinyin-isearch--hieroglyphs-recursion "nu") '((("nuo" "nue" "nuan" "nu"))) ))
    (setq pinyin-isearch-strict nil)
    )
)

(ert-deftest pinyin-isearch--filter-full-variants ()
  (with-temp-buffer
    (should (equal (pinyin-isearch--filter-full-variants 'pinyin-isearch--pinyin-to-hieroglyphs
                                                         '((("na") ("i")))) '((("na") ("i"))) ))
    (should (equal (pinyin-isearch--filter-full-variants 'pinyin-isearch--pinyin-to-hieroglyphs
                                                         '((("n") ("a") ("i")) (("n") ("ai")) (("na") ("i")) (("nai"))))
            '((("n") ("ai")) (("nai"))) ))
    )
)

(ert-deftest pinyin-isearch--convert-to-hieroglyphs ()
  (with-temp-buffer
    (should (equal (pinyin-isearch--convert-to-hieroglyphs '((("zu") ("hna")))) '((("组足族祖租阻卒诅俎镞菹") ("hna"))) ))
    (should (equal (pinyin-isearch--convert-to-hieroglyphs '((("gg")))) '((("gg"))) ))
    (should (equal (pinyin-isearch--convert-to-hieroglyphs '(((".")))) '((("．\\.。・¨…∵∴°⊙"))) ))
    (should (equal (pinyin-isearch--convert-to-hieroglyphs '((("nu")))) '((("女钕恧衄怒努奴弩驽胬孥"))) ))
    (should (equal (pinyin-isearch--convert-to-hieroglyphs '((("nuo" "nue" "nuan" "nu")))) '((("诺挪懦糯喏搦傩锘" "虐疟" "暖" "女钕恧衄怒努奴弩驽胬孥"))) ))
    )
)

(ert-deftest pinyin-isearch--regex-concat-hieroglyphs ()
  (with-temp-buffer
    (should (equal (pinyin-isearch--regex-concat-hieroglyphs '(("阿啊呵腌嗄锕吖") ("嗯唔") (">."))) "[阿啊呵腌嗄锕吖][嗯唔][>.]"))
    (should (equal (pinyin-isearch--regex-concat-hieroglyphs '(("gg"))) "[gg]")) ; no marker
    (should (equal (pinyin-isearch--regex-concat-hieroglyphs
                    (list (list (concat pinyin-isearch--non-syllable-marker-string "gg")))) "gg")) ; with marker
    (should (equal (pinyin-isearch--regex-concat-hieroglyphs
                    '(("诺挪懦糯喏搦傩锘" "虐疟" "暖" "女钕恧衄怒努奴弩驽胬孥"))) "[诺挪懦糯喏搦傩锘虐疟暖女钕恧衄怒努奴弩驽胬孥]"))
    (should (equal (pinyin-isearch--regex-concat-hieroglyphs
                    '(("诺挪懦糯喏搦傩锘" "虐疟" "暖" "女钕恧衄怒努奴弩驽胬孥") ("暖"))) "[诺挪懦糯喏搦傩锘虐疟暖女钕恧衄怒努奴弩驽胬孥][暖]"))
    (should (equal (pinyin-isearch--regex-concat-hieroglyphs
                    (list (list "诺挪懦糯喏搦傩锘" "虐疟" "暖" "女钕恧衄怒努奴弩驽胬孥") (list (concat pinyin-isearch--non-syllable-marker-string "g")))) "[诺挪懦糯喏搦傩锘虐疟暖女钕恧衄怒努奴弩驽胬孥]g"))
    )
)


(ert-deftest pinyin-isearch-hieroglyphs-regexp-function ()
  (with-temp-buffer
    (should (equal (pinyin-isearch-hieroglyphs-regexp-function "nai") "\\([嗯唔][爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹]\\|[乃奶奈耐氖艿鼐佴萘柰]\\)"))
    (should (equal (pinyin-isearch-hieroglyphs-regexp-function "gg") "gg"))
    (should (equal (pinyin-isearch-hieroglyphs-regexp-function ".") "[．\\.。・¨…∵∴°⊙]"))
    (should (equal (pinyin-isearch-hieroglyphs-regexp-function "nu") "[诺挪懦糯喏搦傩锘虐疟暖女钕恧衄怒努奴弩驽胬孥]"))
    (should (equal (pinyin-isearch-hieroglyphs-regexp-function "lu") "[落罗络洛逻裸骆萝螺锣箩摞烙捋珞骡猡镙椤倮蠃荦瘰泺漯脶硌雒论轮伦沦仑抡囵纶略掠锊乱卵峦挛孪栾銮娈滦鸾脔律旅绿率虑履屡侣缕驴吕榈滤捋铝褛闾膂氯稆路陆录卢露鲁炉鹿碌庐芦噜颅禄辘卤虏麓泸赂漉戮簏轳鹭掳潞鲈撸栌垆胪蓼渌鸬逯璐辂橹镥舻氇]"))
    (setq pinyin-isearch-strict t)
    (should (equal (pinyin-isearch-hieroglyphs-regexp-function "gg") ""))
    (should (equal (pinyin-isearch-hieroglyphs-regexp-function ".") "[．。・¨…∵∴°⊙]"))
    (setq pinyin-isearch-strict nil)
    )
)


(provide 'pinyin-isearch-hieroglyphs-tests)
;;; pinyin-isearch-hieroglyphs-tests.el ends here
