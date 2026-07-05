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

(require 'pinyin-isearch-chars)
(require 'ert)


(pinyin-isearch-chars-load)
(defvar pinyin-isearch-strict nil) ; required, because it is set in pinyin-isearch.el
(defvar pinyin-isearch-full-fallback t) ; required, because it is set in pinyin-isearch.el

;; for manually calling
(defun test-pinyin-isearch-chars--get-vars ()
  (print (list "pinyin-isearch-strict" pinyin-isearch-strict))
  (print (list "pinyin-isearch-full-fallback" pinyin-isearch-full-fallback))
  (print (list "pinyin-isearch-default-mode" pinyin-isearch-default-mode))
  (print (list "pinyin-isearch-chars-fallback" pinyin-isearch-chars-fallback)))

;; (with-temp-buffer
;;   (test-pinyin-isearch-chars--get-vars)
;;   ;; (pinyin-isearch-chars--recursion "nin"))
;;   (pinyin-isearch-chars-regexp-function "nin"))
;;   (pinyin-isearch-chars-strict-regexp-function "nin")) ; 您

;; -=-= ---- Tests

(ert-deftest test-pinyin-isearch-chars--rules-to-first-syllable-letters ()
  (with-temp-buffer
    (let ((pinyin-isearch-strict nil))
      (should (equal (pinyin-isearch-chars--rules-to-first-syllable-letters '(("a" "阿啊呵腌嗄锕吖") ("ai" "爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹"))) '(("ai" ("ai")) ("a" ("ai" "a")))))
      )))

(ert-deftest test-pinyin-isearch-chars--get-syllables-by-prefix ()
  (with-temp-buffer
    (let ((pinyin-isearch-strict nil))
      ;; (assoc-string "n" pinyin-isearch-chars--py-punct-rules)))

      (should (equal (pinyin-isearch-chars--get-syllables-by-prefix "a") '("ao" "ang" "an" "ai" "a") ))
      (should (equal (pinyin-isearch-chars--get-syllables-by-prefix ">") '(">") ))
      (should (equal (pinyin-isearch-chars--get-syllables-by-prefix ".") '(".") ))
      (should (equal (pinyin-isearch-chars--get-syllables-by-prefix "g") '("guo" "gun" "gui" "guang" "guan" "guai" "gua" "gu" "gou" "gong" "geng" "gen" "gei" "ge" "gao" "gang" "gan" "gai" "ga") ))
      (should (equal (pinyin-isearch-chars--get-syllables-by-prefix "n") '("nuo" "nue" "nuan" "nu" "nou" "nong" "niu" "ning" "nin" "nie" "niao" "niang" "nian" "ni" "ng" "neng" "nen" "nei" "ne" "nao" "nang" "nan" "nai" "na" "n") ))
      ))
)


(ert-deftest test-pinyin-isearch-chars--pinyin-to-hieroglyphs ()
  (with-temp-buffer
    (let ((pinyin-isearch-strict nil))
    (should (equal (pinyin-isearch-chars--pinyin-to-hieroglyphs "a") "阿啊呵腌嗄锕吖"))
    (should (equal (pinyin-isearch-chars--pinyin-to-hieroglyphs "g") "g"))
    (should (equal (pinyin-isearch-chars--pinyin-to-hieroglyphs ".") "．\\.。・¨…∵∴°⊙"))
    (should (equal (pinyin-isearch-chars--pinyin-to-hieroglyphs "nv") "女钕恧衄")) ; for ppl who likes "v"
    (should (equal (pinyin-isearch-chars--pinyin-to-hieroglyphs "nu") "女钕恧衄怒努奴弩驽胬孥"))
    ))
)
;; 1) MAIN - Split (recursion)
(ert-deftest test-pinyin-isearch-chars--recursion ()
  (progn
    (with-temp-buffer
      (setq pinyin-isearch-strict t) ; working
      (setq pinyin-isearch-chars-fallback nil)
      (setq pinyin-isearch-chars--cached-query nil)
      (should (equal (pinyin-isearch-chars--recursion "i") nil))
      (should (equal (pinyin-isearch-chars--recursion "nunu") '((("nu") ("nu")))))
      (should (not (pinyin-isearch-chars--recursion "gni"))) ; should return nil
      (should (not (pinyin-isearch-chars--recursion "nig")))) ; - should be  nil

    (with-temp-buffer
      (setq pinyin-isearch-strict nil)
      (setq pinyin-isearch-chars-fallback nil)
      (setq pinyin-isearch-chars--cached-query nil)
      (should (equal (pinyin-isearch-chars--recursion "nug") '((("nu") ("guo" "gun" "gui" "guang" "guan" "guai" "gua" "gu" "gou" "gong" "geng" "gen" "gei" "ge" "gao" "gang" "gan" "gai" "ga")))))
      (should (equal (pinyin-isearch-chars--recursion "i") nil))
      (should (equal (pinyin-isearch-chars--recursion "") nil))
      (should (equal (pinyin-isearch-chars--recursion nil) nil))
      (should (equal (pinyin-isearch-chars--recursion "g") '((("guo" "gun" "gui" "guang" "guan" "guai" "gua" "gu" "gou" "gong" "geng" "gen" "gei" "ge" "gao" "gang" "gan" "gai" "ga")))))
      (should (equal (pinyin-isearch-chars--recursion "nuii") nil))
      (should (equal (pinyin-isearch-chars--recursion "nunu") '((("nu") ("nuo" "nue" "nuan" "nu")))))
      (should (equal (pinyin-isearch-chars--recursion "a&&") nil ))
      )

    (with-temp-buffer
      (setq pinyin-isearch-strict nil)
      (setq pinyin-isearch-chars-fallback t)
      (setq pinyin-isearch-chars--cached-query nil)
      (should (equal (pinyin-isearch-chars--recursion "") nil))
      (should (equal (pinyin-isearch-chars--recursion "i") '((("\34i")))))
      (should (equal (pinyin-isearch-chars--recursion nil) nil))
      (should (equal (pinyin-isearch-chars--recursion "nunu") '((("nu") ("n") ("\34u")) (("nu") ("nuo" "nue" "nuan" "nu")) (("nu") ("\34nu")) (("n") ("\34unu")))))
      (should (equal (pinyin-isearch-chars--recursion "nig") '((("ni") ("guo" "gun" "gui" "guang" "guan" "guai" "gua" "gu" "gou" "gong" "geng" "gen" "gei" "ge" "gao" "gang" "gan" "gai" "ga")) (("ni") ("\34g")) (("n") ("\34ig")))))
      (should (equal (pinyin-isearch-chars--recursion "a>") '((("a") (">")) (("a") ("\34>")))))
      (should (equal (pinyin-isearch-chars--recursion "nuii")  '((("nu") ("\34ii")) (("n") ("\34uii")))))
      (should (equal (pinyin-isearch-chars--recursion "a>.") '((("a") (">") ("\34.")) (("a") (">") (".")) (("a") ("\34>.")))))
      (should (equal (pinyin-isearch-chars--recursion "a&&") '((("a") ("\34&&"))) ))
      (should (equal (pinyin-isearch-chars--recursion "an.") '((("an") (".")) (("an") ("\34.")) (("a") ("n") ("\34.")) (("a") ("n") (".")) (("a") ("\34n."))) ))
      (should (equal (pinyin-isearch-chars--recursion "cccc") '((("\34cccc")) )))
      (should (equal (pinyin-isearch-chars--recursion "a") '((("ao" "ang" "an" "ai" "a"))) ))
      (should (equal (pinyin-isearch-chars--recursion "an") '((("ang" "an")) (("a") ("nuo" "nue" "nuan" "nu" "nou" "nong" "niu" "ning" "nin" "nie" "niao" "niang" "nian" "ni" "ng" "neng" "nen" "nei" "ne" "nao" "nang" "nan" "nai" "na" "n")) (("a") ("\34n"))) ))
      (should (equal (pinyin-isearch-chars--recursion "zuana") '((("zuan") ("ao" "ang" "an" "ai" "a")) (("zuan") ("\34a")) (("zu") ("a") ("\34na")) (("zu") ("a") ("nao" "nang" "nan" "nai" "na")) (("zu") ("a") ("n") ("ao" "ang" "an" "ai" "a")) (("zu") ("a") ("n") ("\34a")) (("zu") ("an") ("\34a")) (("zu") ("an") ("ao" "ang" "an" "ai" "a")) (("zu") ("\34ana"))) ))
      (should (equal (pinyin-isearch-chars--recursion "zuhna") '((("zu") ("\34hna"))) ))
      (should (equal (pinyin-isearch-chars--recursion "zu.na") '((("zu") (".") ("\34na")) (("zu") (".") ("nao" "nang" "nan" "nai" "na")) (("zu") (".") ("n") ("ao" "ang" "an" "ai" "a")) (("zu") (".") ("n") ("\34a")) (("zu") ("\34.na"))) ))
      (should (equal (pinyin-isearch-chars--recursion "nai") '((("nai")) (("na") ("\34i")) (("n") ("a") ("\34i")) (("n") ("ai")) (("n") ("\34ai"))) ))
      )

    (with-temp-buffer
      (setq pinyin-isearch-strict t)
      (setq pinyin-isearch-chars-fallback t)
      (setq pinyin-isearch-chars--cached-query nil)
      (should (equal (pinyin-isearch-chars--recursion "") nil))
      (should (equal (pinyin-isearch-chars--recursion nil) nil))
      (should (equal (pinyin-isearch-chars--recursion "a>") '((("a") (">")) (("a") ("\34>"))) ))
      (should (equal (pinyin-isearch-chars--recursion "a>.") '((("a") (">") ("\34.")) (("a") (">") (".")) (("a") ("\34>."))) ))
      (should (equal (pinyin-isearch-chars--recursion "an.") '((("an") (".")) (("an") ("\34.")) (("a") ("n") ("\34.")) (("a") ("n") (".")) (("a") ("\34n."))) ))
      (should (equal (pinyin-isearch-chars--recursion "nai") '((("nai")) (("na") ("\34i")) (("n") ("a") ("\34i")) (("n") ("ai")) (("n") ("\34ai"))) ))
      (should (equal (pinyin-isearch-chars--recursion "a&&") '((("a") ("\34&&"))) ))
      (should (equal (pinyin-isearch-chars--recursion "cccc") nil ))
      (should (equal (pinyin-isearch-chars--recursion "zuhna") '((("zu") ("\34hna"))) ))
      (should (equal (pinyin-isearch-chars--recursion "nu") '((("nu")) (("n") ("\34u"))) ))
      )
  )
  ;; short variang of above
  (progn
    (with-temp-buffer
      ;; STRICT: Only exact matches, fallback disabled
      (setq pinyin-isearch-strict t)
      (setq pinyin-isearch-chars-fallback nil)
      (should (equal (pinyin-isearch-chars--recursion "nunu") '((("nu") ("nu")))))
      (should (equal (pinyin-isearch-chars--recursion "cccc") nil))
      (should (equal (pinyin-isearch-chars--recursion "a>.") '((("a") (">") ("."))))))

    (with-temp-buffer
      ;; STRICT: fallback enabled
      (setq pinyin-isearch-strict t)
      (setq pinyin-isearch-chars-fallback t)
      (should (equal (pinyin-isearch-chars--recursion "a>.") '((("a") (">") ("\34.")) (("a") (">") (".")) (("a") ("\34>.")))))
      (should (equal (pinyin-isearch-chars--recursion "cccc") nil)))

    (with-temp-buffer
      ;; NON-STRICT: fallback disabled
      (setq pinyin-isearch-strict nil)
      (setq pinyin-isearch-chars-fallback nil)
      (should
       (equal
        (pinyin-isearch-chars--recursion "nug")
        '((("nu") ("guo" "gun" "gui" "guang" "guan" "guai" "gua" "gu" "gou" "gong" "geng" "gen" "gei" "ge" "gao" "gang" "gan" "gai" "ga")))))
      (should (equal (pinyin-isearch-chars--recursion "") nil)))

    (with-temp-buffer
      ;; NON-STRICT: fallback enabled
      (setq pinyin-isearch-strict nil)
      (setq pinyin-isearch-chars-fallback t)
      (should (equal (pinyin-isearch-chars--recursion "cccc") '((("\34cccc")))))
      (should (equal
               (pinyin-isearch-chars--recursion "nunu")
               '((("nu") ("n") ("\34u"))
                 (("nu") ("nuo" "nue" "nuan" "nu"))
                 (("nu") ("\34nu"))
                 (("n") ("\34unu")))))
      (should (equal
               (pinyin-isearch-chars--recursion "nig")
               '((("ni") ("guo" "gun" "gui" "guang" "guan" "guai" "gua" "gu" "gou" "gong" "geng" "gen" "gei" "ge" "gao" "gang" "gan" "gai" "ga"))
                 (("ni") ("\34g"))
                 (("n") ("\34ig")))))))
  )

;; not used
;; (ert-deftest test-pinyin-isearch-chars--filter-full-variants ()
;;   (with-temp-buffer
;;     (let ((pinyin-isearch-strict nil))
;;       (should (equal (pinyin-isearch-chars--filter-full-variants 'pinyin-isearch-chars--pinyin-to-hieroglyphs
;;                                                                  '((("na") ("i")))) '((("na") ("i"))) ))
;;       (should (equal (pinyin-isearch-chars--filter-full-variants 'pinyin-isearch-chars--pinyin-to-hieroglyphs
;;                                                                  '((("n") ("a") ("i")) (("n") ("ai")) (("na") ("i")) (("nai"))))
;;                      '((("n") ("ai")) (("nai"))) ))
;;       ))
;; )

(ert-deftest test-pinyin-isearch-chars--convert-to-hieroglyphs ()
  (with-temp-buffer
    (let ((pinyin-isearch-strict nil))
      (should (equal (pinyin-isearch-chars--convert-to-hieroglyphs '((("n")))) '((("嗯唔"))) ))
      (should (equal (pinyin-isearch-chars--convert-to-hieroglyphs '((("zu") ("hna")))) '((("组足族祖租阻卒诅俎镞菹") ("hna"))) ))
      (should (equal (pinyin-isearch-chars--convert-to-hieroglyphs '((("gg")))) '((("gg"))) ))
      (should (equal (pinyin-isearch-chars--convert-to-hieroglyphs '(((".")))) '((("．\\.。・¨…∵∴°⊙"))) ))
      (should (equal (pinyin-isearch-chars--convert-to-hieroglyphs '((("nu")))) '((("女钕恧衄怒努奴弩驽胬孥"))) ))
      (should (equal (pinyin-isearch-chars--convert-to-hieroglyphs '((("nuo" "nue" "nuan" "nu")))) '((("诺挪懦糯喏搦傩锘" "虐疟" "暖" "女钕恧衄怒努奴弩驽胬孥"))) ))
      ))
)

(ert-deftest test-pinyin-isearch-chars--regex-concat-hieroglyphs ()
  (with-temp-buffer
    (should (equal (pinyin-isearch-chars--regex-concat-hieroglyphs '(("阿啊呵腌嗄锕吖") ("嗯唔") (">."))) "[阿啊呵腌嗄锕吖][嗯唔][>.]"))
    (should (equal (pinyin-isearch-chars--regex-concat-hieroglyphs '(("gg"))) "[gg]")) ; no marker
    (should (equal (pinyin-isearch-chars--regex-concat-hieroglyphs
                    (list (list (concat pinyin-isearch-chars--non-syllable-marker-string "gg")))) "gg")) ; with marker
    (should (equal (pinyin-isearch-chars--regex-concat-hieroglyphs
                    '(("诺挪懦糯喏搦傩锘" "虐疟" "暖" "女钕恧衄怒努奴弩驽胬孥")))
                   "[诺挪懦糯喏搦傩锘虐疟暖女钕恧衄怒努奴弩驽胬孥]"))
    (should (equal (pinyin-isearch-chars--regex-concat-hieroglyphs
                    '(("诺挪懦糯喏搦傩锘" "虐疟" "暖" "女钕恧衄怒努奴弩驽胬孥") ("暖")))
                   "[诺挪懦糯喏搦傩锘虐疟暖女钕恧衄怒努奴弩驽胬孥]暖"))
    (should (equal (pinyin-isearch-chars--regex-concat-hieroglyphs
                    (list (list "诺挪懦糯喏搦傩锘" "虐疟" "暖" "女钕恧衄怒努奴弩驽胬孥") (list (concat pinyin-isearch-chars--non-syllable-marker-string "g")))) "[诺挪懦糯喏搦傩锘虐疟暖女钕恧衄怒努奴弩驽胬孥]g"))
    (should (equal (pinyin-isearch-chars--regex-concat-hieroglyphs '(("昂") ("肮") ("盎"))) "昂肮盎"))
    (should (equal (pinyin-isearch-chars--regex-concat-hieroglyphs '(("昂肮"))) "[昂肮]"))
    (should (equal (pinyin-isearch-chars--regex-concat-hieroglyphs '(("昂" "肮"))) "[昂肮]"))
    (should (equal (pinyin-isearch-chars--regex-concat-hieroglyphs '(("X" "Y"))) "[XY]"))
    (should (equal (pinyin-isearch-chars--regex-concat-hieroglyphs '(("\x1c昂"))) "昂"))
    (should (equal (pinyin-isearch-chars--regex-concat-hieroglyphs '(("\x1c肮" "\x1c盎"))) "肮盎"))

    (should (equal (pinyin-isearch-chars--regex-concat-hieroglyphs '(("嗯唔") ("nuii" "\x1cuii"))) "[嗯唔][nuii]uii"))
    (should (equal (pinyin-isearch-chars--regex-concat-hieroglyphs '(("昂") ("肮" "阿") ("盎"))) "昂[肮阿]盎"))

    (should (equal (pinyin-isearch-chars--regex-concat-hieroglyphs '(("嗯唔") ("nuii" "uii"))) "[嗯唔][nuii]uii"))
    (should (equal (let ((v '((("昂肮盎" "昂肮盎") ("奥澳傲熬敖凹袄懊坳嗷拗鏖骜鳌翱岙廒遨獒聱媪螯鏊" "昂肮盎" "安案按暗岸俺谙黯鞍氨庵桉鹌胺铵揞犴埯" "爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹" "阿啊呵腌嗄锕吖")))))
                     (mapcar #'pinyin-isearch-chars--regex-concat-hieroglyphs v))
                   '("[昂肮盎昂肮盎][奥澳傲熬敖凹袄懊坳嗷拗鏖骜鳌翱岙廒遨獒聱媪螯鏊昂肮盎安案按暗岸俺谙黯鞍氨庵桉鹌胺铵揞犴埯爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹阿啊呵腌嗄锕吖]")))
    (should (equal (pinyin-isearch-chars--regex-concat-hieroglyphs '(("\34a"))) "a"))))


(ert-deftest test-pinyin-isearch-chars--concat-hieroglyphs ()
  (with-temp-buffer
    (let ((pinyin-isearch-strict nil)
          (pinyin-isearch-chars-fallback t)
          (pinyin-isearch-full-fallback t))
      (should (equal
               (pinyin-isearch-chars--concat-variants
                "a"
                '((("奥澳傲熬敖凹袄懊坳嗷拗鏖骜鳌翱岙廒遨獒聱媪螯鏊" "昂肮盎" "安案按暗岸俺谙黯鞍氨庵桉鹌胺铵揞犴埯" "爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹" "阿啊呵腌嗄锕吖"))))
               "\\(a\\|[奥澳傲熬敖凹袄懊坳嗷拗鏖骜鳌翱岙廒遨獒聱媪螯鏊昂肮盎安案按暗岸俺谙黯鞍氨庵桉鹌胺铵揞犴埯爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹阿啊呵腌嗄锕吖]\\)"))
      (should (equal
               (pinyin-isearch-chars--concat-variants
                "a"
                '((("昂肮盎" "昂") ("奥澳傲熬敖凹袄懊坳嗷拗鏖骜鳌翱岙廒遨獒聱媪螯鏊" "昂肮盎" "安案按暗岸俺谙黯鞍氨庵桉鹌胺铵揞犴埯" "爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹" "阿啊呵腌嗄锕吖"))))
               "\\(a\\|[昂肮盎昂][奥澳傲熬敖凹袄懊坳嗷拗鏖骜鳌翱岙廒遨獒聱媪螯鏊昂肮盎安案按暗岸俺谙黯鞍氨庵桉鹌胺铵揞犴埯爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹阿啊呵腌嗄锕吖]\\)"
      ))
      (should (equal
               (pinyin-isearch-chars--concat-variants "nuii" '((("女钕恧衄怒努奴弩驽胬孥") ("ii")) (("嗯唔") ("nuii" "uii"))))
               "\\(nuii\\|[女钕恧衄怒努奴弩驽胬孥]ii\\|[嗯唔][nuii]uii\\)"
               ))
      (should (equal
               (pinyin-isearch-chars--concat-variants "nuii" '((("nu") ("\34ii")) (("nvv") ("\34uii"))))
      "\\(nuii\\|[nu]ii\\|[nvv]uii\\)"
      )))))


;; -=-= maptree

(ert-deftest test-pinyin-isearch-chars--maptree ()
  (should (equal (pinyin-isearch-chars--maptree
                  #'pinyin-isearch-chars--pinyin-to-hieroglyphs
                  '(("i")))
                 '(("i"))))
  (should (equal (pinyin-isearch-chars--maptree
                  #'pinyin-isearch-chars--pinyin-to-hieroglyphs
                  '(("n")))
                 '(("嗯唔"))))
  (should (equal
           (pinyin-isearch-chars--maptree
            #'pinyin-isearch-chars--pinyin-to-hieroglyphs
            nil)
           nil))

  (should (equal
           (pinyin-isearch-chars--maptree
            #'pinyin-isearch-chars--pinyin-to-hieroglyphs
            '((("ao" "ang" "an" "ai" "a"))))
           '((("奥澳傲熬敖凹袄懊坳嗷拗鏖骜鳌翱岙廒遨獒聱媪螯鏊" "昂肮盎" "安案按暗岸俺谙黯鞍氨庵桉鹌胺铵揞犴埯" "爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹" "阿啊呵腌嗄锕吖")))))
  )

;; -=-= MAIN final - test: chars-regexp-function

(ert-deftest test-pinyin-isearch-chars-regexp-function ()
  (with-temp-buffer
    (setq pinyin-isearch-strict nil)
    (setq pinyin-isearch-chars-fallback nil)
    (setq pinyin-isearch-full-fallback nil)
    (setq pinyin-isearch-chars--cached-query nil)
    (should (equal (pinyin-isearch-chars--concat-variants
                    "i"
                    (pinyin-isearch-chars--maptree ;; 2) convert-to-hieroglyphs - for nil return nil, or '((("嗯唔")))
                     #'pinyin-isearch-chars--pinyin-to-hieroglyphs
                     (pinyin-isearch-chars--recursion "i") ;; 1) split - may return nil
                     )) "$^"))
    (should (equal (pinyin-isearch-chars-regexp-function "i") "$^"))
    )

  (with-temp-buffer
    (setq pinyin-isearch-strict nil)
    (setq pinyin-isearch-chars-fallback t)
    (setq pinyin-isearch-full-fallback t)
    (setq pinyin-isearch-chars--cached-query nil)
    (should (equal (pinyin-isearch-chars-regexp-function "a")
                   "\\(a\\|[奥澳傲熬敖凹袄懊坳嗷拗鏖骜鳌翱岙廒遨獒聱媪螯鏊昂肮盎安案按暗岸俺谙黯鞍氨庵桉鹌胺铵揞犴埯爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹阿啊呵腌嗄锕吖]\\)"))
    (should (equal (pinyin-isearch-chars--maptree ;; 2) convert-to-hieroglyphs - for nil return nil, or '((("嗯唔")))
                     #'pinyin-isearch-chars--pinyin-to-hieroglyphs
                     (pinyin-isearch-chars--recursion "a") ;; 1) split - may return nil
                     )
                   '((("奥澳傲熬敖凹袄懊坳嗷拗鏖骜鳌翱岙廒遨獒聱媪螯鏊" "昂肮盎" "安案按暗岸俺谙黯鞍氨庵桉鹌胺铵揞犴埯" "爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹" "阿啊呵腌嗄锕吖")))))

    (should (equal
             (pinyin-isearch-chars--concat-variants
              "i"
              (pinyin-isearch-chars--maptree ;; 2) convert-to-hieroglyphs - for nil return nil, or '((("嗯唔")))
               #'pinyin-isearch-chars--pinyin-to-hieroglyphs
               (pinyin-isearch-chars--recursion "i") ;; 1) split - may return nil
               ))
             "i"))
    )

  (with-temp-buffer
    (setq pinyin-isearch-strict nil)
    (setq pinyin-isearch-chars-fallback nil)
    (setq pinyin-isearch-chars--cached-query nil)
    (setq pinyin-isearch-full-fallback nil)
    (should (equal
             (pinyin-isearch-chars--maptree ;; 2) convert-to-hieroglyphs - for nil return nil, or '((("嗯唔")))
              #'pinyin-isearch-chars--pinyin-to-hieroglyphs
              (pinyin-isearch-chars--recursion "i") ;; 1) split - may return nil
              ) nil))
    (should (equal (pinyin-isearch-chars--concat-variants
                    "i"
                    (pinyin-isearch-chars--maptree ;; 2) convert-to-hieroglyphs - for nil return nil, or '((("嗯唔")))
                     #'pinyin-isearch-chars--pinyin-to-hieroglyphs
                     (pinyin-isearch-chars--recursion "i") ;; 1) split - may return nil
                     )) "$^"))
    (should (equal (pinyin-isearch-chars-regexp-function "i") "$^"))

    (should (equal (pinyin-isearch-chars-regexp-function "nuii") "$^"))
    (should (equal (pinyin-isearch-chars-regexp-function "task") "$^"))
    (should (equal (pinyin-isearch-chars-regexp-function "nunu") "[女钕恧衄怒努奴弩驽胬孥][诺挪懦糯喏搦傩锘虐疟暖女钕恧衄怒努奴弩驽胬孥]"))
    (should (equal (pinyin-isearch-chars-regexp-function "nig") "[你呢尼泥逆倪匿拟腻妮霓昵溺旎睨鲵坭猊怩伲祢慝铌][国过果锅郭裹帼蝈聒馘掴埚虢呙崞猓椁蜾滚棍鲧绲磙辊衮规归贵鬼桂跪柜轨瑰诡刽龟硅闺皈傀癸圭晷簋妫鲑匦庋宄炔刿桧炅鳜广光逛犷咣胱桄关观管官馆惯冠贯罐灌棺莞倌纶掼盥涫鳏鹳怪乖拐掴挂瓜刮寡呱褂卦剐鸹栝胍诖故古顾股鼓姑骨固孤谷估雇辜咕沽箍菇汩轱锢蛊梏鸪毂鹄臌瞽罟钴觚鹘菰蛄嘏诂崮酤牿牯痼鲴够购构狗沟勾苟钩觏篝垢佝岣诟鞲笱枸遘媾缑彀工公共供功攻宫贡恭巩躬龚弓拱肱汞蚣珙觥更耿耕颈庚羹梗哽赓鲠埂绠根跟亘艮哏茛给个革各歌格哥戈隔葛割阁胳搁疙咯鸽嗝骼颌屹搿膈镉纥袼仡鬲塥圪哿舸铬硌虼高告稿搞糕膏皋羔睾槁藁缟篙镐诰槔杲郜锆刚港钢岗纲缸扛杠冈肛罡戆筻感干敢赶甘肝杆尴赣橄竿秆擀坩苷柑泔矸澉疳酐淦绀旰改该概盖丐钙赅溉垓陔戤咖尬嘎噶轧伽旮钆尕尜]"))


    (should (equal (pinyin-isearch-chars-regexp-function "a>") "[阿啊呵腌嗄锕吖][＞〉》≯≥]"))
    (should (equal (pinyin-isearch-chars-regexp-function "nai") "\\([乃奶奈耐氖艿鼐佴萘柰]\\|[嗯唔][爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹]\\)"))
    (should (equal (pinyin-isearch-chars-regexp-function "n") "[诺挪懦糯喏搦傩锘虐疟暖女钕恧衄怒努奴弩驽胬孥耨农弄浓侬哝脓牛纽扭妞钮拗忸狃宁凝拧泞咛狞柠佞聍苎甯您恁捏涅聂孽蹑嗫啮镊镍乜陧颞臬蘖鸟尿袅嬲茑脲娘酿年念廿粘碾捻蔫撵拈黏鲶鲇辇埝你呢尼泥逆倪匿拟腻妮霓昵溺旎睨鲵坭猊怩伲祢慝铌嗯唔能嫩恁内馁呢讷脑闹恼挠瑙淖呶猱铙孬硇蛲垴囊囔馕攮曩难南男楠喃囡囝腩蝻赧乃奶奈耐氖艿鼐佴萘柰那拿呢哪纳娜呐捺钠镎肭衲嗯唔]"))
    (should (equal (pinyin-isearch-chars-regexp-function "ggg") "$^"))
    (should (equal (pinyin-isearch-chars-regexp-function "vi") "$^"))
    (should (equal (pinyin-isearch-chars-regexp-function ".") "[．\\.。・¨…∵∴°⊙]"))
    (should (equal (pinyin-isearch-chars-regexp-function "nu") "[诺挪懦糯喏搦傩锘虐疟暖女钕恧衄怒努奴弩驽胬孥]"))
    (should (equal (pinyin-isearch-chars-regexp-function "lu") "[落罗络洛逻裸骆萝螺锣箩摞烙捋珞骡猡镙椤倮蠃荦瘰泺漯脶硌雒论轮伦沦仑抡囵纶略掠锊乱卵峦挛孪栾銮娈滦鸾脔律旅绿率虑履屡侣缕驴吕榈滤捋铝褛闾膂氯稆路陆录卢露鲁炉鹿碌庐芦噜颅禄辘卤虏麓泸赂漉戮簏轳鹭掳潞鲈撸栌垆胪蓼渌鸬逯璐辂橹镥舻氇]"))
    (should (equal (pinyin-isearch-chars-regexp-function "nui") "$^")) ; "\\([嗯唔]ui\\|[女钕恧衄怒努奴弩驽胬孥]i\\)"
    ;; check for change of `pinyin-isearch-strict':
    ;; ! sk at the end, because there is no characters starting with sk !
    (should (equal (pinyin-isearch-chars-regexp-function "task") "$^"))
    )

  (with-temp-buffer
    (setq pinyin-isearch-strict t)
    (setq pinyin-isearch-chars-fallback nil)
    (setq pinyin-isearch-chars--cached-query nil)
    (setq pinyin-isearch-full-fallback nil)
    (should (equal (pinyin-isearch-chars-regexp-function "nuii") "$^"))
    (should (equal (pinyin-isearch-chars-regexp-function "task") "$^"))
    (should (equal (pinyin-isearch-chars-regexp-function "gg") "$^"))
    (should (equal (pinyin-isearch-chars-regexp-function ".") "[．。・¨…∵∴°⊙]"))
    (should (equal (pinyin-isearch-chars-regexp-function "nu") "[女钕恧衄怒努奴弩驽胬孥]"))
    (should (equal (pinyin-isearch-chars-regexp-function "lu") "[律旅绿率虑履屡侣缕驴吕榈滤捋铝褛闾膂氯稆路陆录卢露鲁炉鹿碌庐芦噜颅禄辘卤虏麓泸赂漉戮簏轳鹭掳潞鲈撸栌垆胪蓼渌鸬逯璐辂橹镥舻氇]"))
    (should (equal (pinyin-isearch-chars-regexp-function "nui") "$^"))
    (should (equal (pinyin-isearch-chars-regexp-function "nunu") "[女钕恧衄怒努奴弩驽胬孥][女钕恧衄怒努奴弩驽胬孥]"))
    (should (equal (pinyin-isearch-chars-regexp-function "nuai") "[女钕恧衄怒努奴弩驽胬孥][爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹]"))
    )
  (with-temp-buffer
    (setq pinyin-isearch-strict t)
    (setq pinyin-isearch-chars-fallback nil)
    (setq pinyin-isearch-chars--cached-query nil)
    (setq pinyin-isearch-full-fallback t)
    (should (equal (pinyin-isearch-chars-regexp-function "naig") "naig"))
    (should (equal (pinyin-isearch-chars-regexp-function "i") "i"))
    (should (equal (pinyin-isearch-chars-regexp-function "nai") "\\(nai\\|[乃奶奈耐氖艿鼐佴萘柰]\\|[嗯唔][爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹]\\)"))
    )

    (with-temp-buffer
      (setq pinyin-isearch-strict nil)
      (setq pinyin-isearch-chars-fallback t)
      (setq pinyin-isearch-full-fallback t)
      (setq pinyin-isearch-chars--cached-query nil)
      (pinyin-isearch-chars-regexp-function "i") ; TODO! we need "\34^$"
      (should (equal (pinyin-isearch-chars-regexp-function "i") "i"))
      (should (equal (pinyin-isearch-chars-regexp-function ".") "\\(.\\|[．\\.。・¨…∵∴°⊙]\\)"))
      (should (equal (pinyin-isearch-chars-regexp-function "nig")
              "\\(nig\\|[你呢尼泥逆倪匿拟腻妮霓昵溺旎睨鲵坭猊怩伲祢慝铌][国过果锅郭裹帼蝈聒馘掴埚虢呙崞猓椁蜾滚棍鲧绲磙辊衮规归贵鬼桂跪柜轨瑰诡刽龟硅闺皈傀癸圭晷簋妫鲑匦庋宄炔刿桧炅鳜广光逛犷咣胱桄关观管官馆惯冠贯罐灌棺莞倌纶掼盥涫鳏鹳怪乖拐掴挂瓜刮寡呱褂卦剐鸹栝胍诖故古顾股鼓姑骨固孤谷估雇辜咕沽箍菇汩轱锢蛊梏鸪毂鹄臌瞽罟钴觚鹘菰蛄嘏诂崮酤牿牯痼鲴够购构狗沟勾苟钩觏篝垢佝岣诟鞲笱枸遘媾缑彀工公共供功攻宫贡恭巩躬龚弓拱肱汞蚣珙觥更耿耕颈庚羹梗哽赓鲠埂绠根跟亘艮哏茛给个革各歌格哥戈隔葛割阁胳搁疙咯鸽嗝骼颌屹搿膈镉纥袼仡鬲塥圪哿舸铬硌虼高告稿搞糕膏皋羔睾槁藁缟篙镐诰槔杲郜锆刚港钢岗纲缸扛杠冈肛罡戆筻感干敢赶甘肝杆尴赣橄竿秆擀坩苷柑泔矸澉疳酐淦绀旰改该概盖丐钙赅溉垓陔戤咖尬嘎噶轧伽旮钆尕尜]\\|[你呢尼泥逆倪匿拟腻妮霓昵溺旎睨鲵坭猊怩伲祢慝铌]g\\|[嗯唔]ig\\)"
              ))

      (should (equal (pinyin-isearch-chars-regexp-function "a&&") "\\(a&&\\|[阿啊呵腌嗄锕吖]&&\\)"))
      (should (equal (pinyin-isearch-chars-regexp-function "a>") "\\(a>\\|[阿啊呵腌嗄锕吖][＞〉》≯≥]\\|[阿啊呵腌嗄锕吖]>\\)"))
      (should (equal (pinyin-isearch-chars-regexp-function "ggg") "ggg"))
      (should (equal (pinyin-isearch-chars-regexp-function "nui") "\\(nui\\|[女钕恧衄怒努奴弩驽胬孥]i\\|[嗯唔]ui\\)"))
      (should (equal (pinyin-isearch-chars-regexp-function "nuii") "\\(nuii\\|[女钕恧衄怒努奴弩驽胬孥]ii\\|[嗯唔]uii\\)"))
      (should (equal (pinyin-isearch-chars-regexp-function "task") "\\(task\\|[他她它踏塔塌榻嗒蹋沓遢挞鳎闼铊趿漯溻獭]sk\\)"))
      )
    )


(ert-deftest test-pinyin-isearch-chars-strict-regexp-function ()
  (with-temp-buffer
    (let ((pinyin-isearch-strict nil))
      (should (equal (pinyin-isearch-chars-strict-regexp-function "gg") "$^"))
      (should (equal (pinyin-isearch-chars-strict-regexp-function ".") "[．。・¨…∵∴°⊙]"))
      (should (equal (pinyin-isearch-chars-strict-regexp-function "nu") "[女钕恧衄怒努奴弩驽胬孥]"))
      (should (equal (pinyin-isearch-chars-strict-regexp-function "lu") "[律旅绿率虑履屡侣缕驴吕榈滤捋铝褛闾膂氯稆路陆录卢露鲁炉鹿碌庐芦噜颅禄辘卤虏麓泸赂漉戮簏轳鹭掳潞鲈撸栌垆胪蓼渌鸬逯璐辂橹镥舻氇]"))
      (should (equal (pinyin-isearch-chars-strict-regexp-function "nui") "$^"))
      (should (equal (pinyin-isearch-chars-strict-regexp-function "nuai") "[女钕恧衄怒努奴弩驽胬孥][爱哀挨碍埃癌艾唉矮哎皑蔼隘暧霭捱嗳瑷嫒锿嗌砹]"))
      )))


(provide 'pinyin-isearch-chars-tests)
;;; pinyin-isearch-chars-tests.el ends here
