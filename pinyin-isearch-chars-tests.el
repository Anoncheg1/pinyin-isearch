;;; pinyin-isearch-chars-tests.el --- Tests for pinyin-isearch chars mode      -*- lexical-binding: t; -*-

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
(require 'pinyin-isearch-chars)

(declare-function pinyin-isearch-chars-regexp-function "pinyin-isearch-chars" (string &optional lax))

(declare-function pinyin-isearch--chars-recursion "pinyin-isearch-chars" (st))
(declare-function pinyin-isearch-chars--recursion "pinyin-isearch-chars" (st))



(ert-deftest pinyin-isearch-chars--rules-to-first-syllable-letters ()
  (with-temp-buffer
    (should (equal (pinyin-isearch-chars--rules-to-first-syllable-letters '(("a" "阿啊呵腌嗄锕吖") ("ai" "爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹"))) '(("ai" ("ai")) ("a" ("ai" "a")))))
    )
)

(ert-deftest pinyin-isearch-chars--get-syllables-by-prefix ()
  (with-temp-buffer
    (should (equal (pinyin-isearch-chars--get-syllables-by-prefix "a") '("ao" "ang" "an" "ai" "a") ))
    (should (equal (pinyin-isearch-chars--get-syllables-by-prefix ">") '(">") ))
    (should (equal (pinyin-isearch-chars--get-syllables-by-prefix ".") '(".") ))
    (should (equal (pinyin-isearch-chars--get-syllables-by-prefix "n") '("nuo" "nue" "nuan" "nu" "nou" "nong" "niu" "ning" "nin" "nie" "niao" "niang" "nian" "ni" "ng" "neng" "nen" "nei" "ne" "nao" "nang" "nan" "nai" "na" "n") ))
    )
)

(ert-deftest pinyin-isearch-chars--pinyin-to-hieroglyphs ()
  (with-temp-buffer
    (should (equal (pinyin-isearch-chars--pinyin-to-hieroglyphs "a") "阿啊呵腌嗄锕吖"))
    (should (equal (pinyin-isearch-chars--pinyin-to-hieroglyphs "g") "g"))
    (should (equal (pinyin-isearch-chars--pinyin-to-hieroglyphs ".") "．\\.。・¨…∵∴°⊙"))
    (should (equal (pinyin-isearch-chars--pinyin-to-hieroglyphs "nv") "女钕恧衄")) ; for ppl who likes "v"
    (should (equal (pinyin-isearch-chars--pinyin-to-hieroglyphs "nu") "女钕恧衄怒努奴弩驽胬孥"))
    )
)

(ert-deftest pinyin-isearch-chars--recursion ()
  (with-temp-buffer
    (should (equal (pinyin-isearch-chars--recursion "a>") '((("a") (">"))) ))
    (should (equal (pinyin-isearch-chars--recursion "a>.") '((("a") (">") ("."))) ))
    (should (equal (pinyin-isearch-chars--recursion "a&&") '((("a")("\34&&"))) ))
    (should (equal (pinyin-isearch-chars--recursion "an.") '((("a") ("n") (".")) (("an") ("."))) ))
    (should (equal (pinyin-isearch-chars--recursion "cccc") '((("\34cccc"))) ))
    (should (equal (pinyin-isearch-chars--recursion "a") '((("ao" "ang" "an" "ai" "a"))) ))
    (should (equal (pinyin-isearch-chars--recursion "an") '((("a") ("nuo" "nue" "nuan" "nu" "nou" "nong" "niu" "ning" "nin" "nie" "niao" "niang" "nian" "ni" "ng" "neng" "nen" "nei" "ne" "nao" "nang" "nan" "nai" "na" "n")) (("ang" "an"))) ))
    (should (equal (pinyin-isearch-chars--recursion "zuana") '((("zu") ("a") ("n") ("ao" "ang" "an" "ai" "a")) (("zu") ("a") ("nao" "nang" "nan" "nai" "na")) (("zu") ("an") ("ao" "ang" "an" "ai" "a")) (("zuan") ("ao" "ang" "an" "ai" "a"))) ))
    (should (equal (pinyin-isearch-chars--recursion "zuhna") '((("zu") ("\34hna"))) ))
    (should (equal (pinyin-isearch-chars--recursion "zu.na") '((("zu") (".") ("n") ("ao" "ang" "an" "ai" "a")) (("zu") (".") ("nao" "nang" "nan" "nai" "na"))) ))
    (should (equal (pinyin-isearch-chars--recursion "nai") '((("n") ("a") ("\34i")) (("n") ("ai")) (("na") ("\34i")) (("nai"))) ))
    (setq pinyin-isearch-strict t)
    (should (equal (pinyin-isearch-chars--recursion "a&&") nil ))
    (should (equal (pinyin-isearch-chars--recursion "cccc") nil ))
    (should (equal (pinyin-isearch-chars--recursion "zuhna") nil ))
    (should (equal (pinyin-isearch-chars--recursion "nai") '((("n") ("ai")) (("nai"))) ))
    (should (equal (pinyin-isearch-chars--recursion "nu") '((("nu"))) ))
    (setq pinyin-isearch-strict nil)
    )
)

(ert-deftest pinyin-isearch-chars--filter-full-variants ()
  (with-temp-buffer
    (should (equal (pinyin-isearch-chars--filter-full-variants 'pinyin-isearch-chars--pinyin-to-hieroglyphs
                                                         '((("na") ("i"))) "nai") '((("na") ("i"))) ))
    (should (equal (pinyin-isearch-chars--filter-full-variants 'pinyin-isearch-chars--pinyin-to-hieroglyphs
                                                         '((("n") ("a") ("i")) (("n") ("ai")) (("na") ("i")) (("nai"))) "nai")
            '((("n") ("ai")) (("nai"))) ))


    )
)

;; (ert-deftest pinyin-isearch-chars--filter-full-variants-and-characters ()
;;   (with-temp-buffer
;;     (should (equal (pinyin-isearch-chars--filter-full-variants-and-characters 'pinyin-isearch-chars--pinyin-to-hieroglyphs
;;                                                          '((("na") ("i")))) nil ))
;;     (should (equal (pinyin-isearch-chars--filter-full-variants-and-characters 'pinyin-isearch-chars--pinyin-to-hieroglyphs
;;                                                          '((("n") ("a") ("i")) (("n") ("ai")) (("na") ("i")) (("nai"))))
;;             '((("n") ("ai")) (("nai"))) ))
;;     (should (equal (pinyin-isearch-chars--filter-full-variants-and-characters 'pinyin-isearch-chars--pinyin-to-hieroglyphs
;;                                                                         '((("zu") ("a") ("n") ("ai")) (("zu") ("a") ("nao" "nang" "nan" "nai" "na")) (("zu") ("an") ("ao" "ang" "an" "ai" "a")) (("zuan") ("ao" "ang" "an" "ai" "a"))))
;;                    '((("zu") ("a") ("n") ("ai"))) ))
;;     )
;; )

(ert-deftest pinyin-isearch-chars--convert-to-hieroglyphs ()
  (with-temp-buffer
    (should (equal (pinyin-isearch-chars--convert-to-hieroglyphs '((("zu") ("hna")))) '((("组足族祖租阻卒诅俎镞菹") ("hna"))) ))
    (should (equal (pinyin-isearch-chars--convert-to-hieroglyphs '((("gg")))) '((("gg"))) ))
    (should (equal (pinyin-isearch-chars--convert-to-hieroglyphs '(((".")))) '((("．\\.。・¨…∵∴°⊙"))) ))
    (should (equal (pinyin-isearch-chars--convert-to-hieroglyphs '((("nu")))) '((("女钕恧衄怒努奴弩驽胬孥"))) ))
    (should (equal (pinyin-isearch-chars--convert-to-hieroglyphs '((("nuo" "nue" "nuan" "nu")))) '((("诺挪懦糯喏搦傩锘" "虐疟" "暖" "女钕恧衄怒努奴弩驽胬孥"))) ))
    )
)

(ert-deftest pinyin-isearch-chars--regex-concat-hieroglyphs ()
  (with-temp-buffer
    (should (equal (pinyin-isearch-chars--regex-concat-hieroglyphs '(("阿啊呵腌嗄锕吖") ("嗯唔") (">."))) "[阿啊呵腌嗄锕吖][嗯唔][>.]"))
    (should (equal (pinyin-isearch-chars--regex-concat-hieroglyphs '(("gg"))) "[gg]")) ; no marker
    (should (equal (pinyin-isearch-chars--regex-concat-hieroglyphs
                    (list (list (concat pinyin-isearch-chars--non-syllable-marker-string "gg")))) "gg")) ; with marker
    (should (equal (pinyin-isearch-chars--regex-concat-hieroglyphs
                    '(("诺挪懦糯喏搦傩锘" "虐疟" "暖" "女钕恧衄怒努奴弩驽胬孥"))) "[诺挪懦糯喏搦傩锘虐疟暖女钕恧衄怒努奴弩驽胬孥]"))
    (should (equal (pinyin-isearch-chars--regex-concat-hieroglyphs
                    '(("诺挪懦糯喏搦傩锘" "虐疟" "暖" "女钕恧衄怒努奴弩驽胬孥") ("暖"))) "[诺挪懦糯喏搦傩锘虐疟暖女钕恧衄怒努奴弩驽胬孥][暖]"))
    (should (equal (pinyin-isearch-chars--regex-concat-hieroglyphs
                    (list (list "诺挪懦糯喏搦傩锘" "虐疟" "暖" "女钕恧衄怒努奴弩驽胬孥") (list (concat pinyin-isearch-chars--non-syllable-marker-string "g")))) "[诺挪懦糯喏搦傩锘虐疟暖女钕恧衄怒努奴弩驽胬孥]g"))
    )
)


(ert-deftest pinyin-isearch-chars-regexp-function ()
  (with-temp-buffer
    (should (equal (pinyin-isearch-chars-regexp-function "nai") "\\([嗯唔][爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹]\\|[乃奶奈耐氖艿鼐佴萘柰]\\)"))
    (should (equal (pinyin-isearch-chars-regexp-function "ggg") "ggg"))
    (should (equal (pinyin-isearch-chars-regexp-function "vi") "vi"))
    (should (equal (pinyin-isearch-chars-regexp-function "task") "\\(task\\|[他她它踏塔塌榻嗒蹋沓遢挞鳎闼铊趿漯溻獭]sk\\)"))
    (should (equal (pinyin-isearch-chars-regexp-function ".") "[．\\.。・¨…∵∴°⊙]"))
    (should (equal (pinyin-isearch-chars-regexp-function "nu") "[诺挪懦糯喏搦傩锘虐疟暖女钕恧衄怒努奴弩驽胬孥]"))
    (should (equal (pinyin-isearch-chars-regexp-function "lu") "[落罗络洛逻裸骆萝螺锣箩摞烙捋珞骡猡镙椤倮蠃荦瘰泺漯脶硌雒论轮伦沦仑抡囵纶略掠锊乱卵峦挛孪栾銮娈滦鸾脔律旅绿率虑履屡侣缕驴吕榈滤捋铝褛闾膂氯稆路陆录卢露鲁炉鹿碌庐芦噜颅禄辘卤虏麓泸赂漉戮簏轳鹭掳潞鲈撸栌垆胪蓼渌鸬逯璐辂橹镥舻氇]"))
    (should (equal (pinyin-isearch-chars-regexp-function "nui") "\\(nui\\|[嗯唔]ui\\|[女钕恧衄怒努奴弩驽胬孥]i\\)")) ; "\\([嗯唔]ui\\|[女钕恧衄怒努奴弩驽胬孥]i\\)"
    (setq pinyin-isearch-strict t)
    (should (equal (pinyin-isearch-chars-regexp-function "gg") "$^"))
    (should (equal (pinyin-isearch-chars-regexp-function ".") "[．。・¨…∵∴°⊙]"))
    (should (equal (pinyin-isearch-chars-regexp-function "nu") "[女钕恧衄怒努奴弩驽胬孥]"))
    (should (equal (pinyin-isearch-chars-regexp-function "lu") "[律旅绿率虑履屡侣缕驴吕榈滤捋铝褛闾膂氯稆路陆录卢露鲁炉鹿碌庐芦噜颅禄辘卤虏麓泸赂漉戮簏轳鹭掳潞鲈撸栌垆胪蓼渌鸬逯璐辂橹镥舻氇]"))
    (should (equal (pinyin-isearch-chars-regexp-function "nui") "$^"))
    (should (equal (pinyin-isearch-chars-regexp-function "nuai") "[女钕恧衄怒努奴弩驽胬孥][爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹]"))
    (setq pinyin-isearch-strict nil)
    )
)

(ert-deftest pinyin-isearch-chars-strict-regexp-function ()
  (with-temp-buffer
    (should (equal (pinyin-isearch-chars-strict-regexp-function "gg") "$^"))
    (should (equal (pinyin-isearch-chars-strict-regexp-function ".") "[．。・¨…∵∴°⊙]"))
    (should (equal (pinyin-isearch-chars-strict-regexp-function "nu") "[女钕恧衄怒努奴弩驽胬孥]"))
    (should (equal (pinyin-isearch-chars-strict-regexp-function "lu") "[律旅绿率虑履屡侣缕驴吕榈滤捋铝褛闾膂氯稆路陆录卢露鲁炉鹿碌庐芦噜颅禄辘卤虏麓泸赂漉戮簏轳鹭掳潞鲈撸栌垆胪蓼渌鸬逯璐辂橹镥舻氇]"))
    (should (equal (pinyin-isearch-chars-strict-regexp-function "nui") "$^"))
    (should (equal (pinyin-isearch-chars-strict-regexp-function "nuai") "[女钕恧衄怒努奴弩驽胬孥][爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹]"))
))


(provide 'pinyin-isearch-chars-tests)
;;; pinyin-isearch-chars-tests.el ends here
