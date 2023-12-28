# pinyin-isearch
Emacs package with minor mode that allow search pinyin ignoring tones.

Again, allow to search with pinyin in pinyin but ignore tone diacritical marks for speed.

It replaces isearch-search-fun-function with our function that replace
 search string with regex that ignore tones.

For example, to find "Shànghǎi" in text you just type: C-s shanghai.

Features:
- white spaces are ignored between syllables
- tone required only in first syllable in text: Zhēn de ma

# install
copy file to ~/.emacs.d/contrib/pinyin-isearch.el

add to ~/.emacs

```elisp
(add-to-list 'load-path "~/.emacs.d/contrib/")
(require 'pinyin-isearch)
```

# activate
``` M-x pinyin-isearch-mode ``` To activate per file: ```  -*- mode: pinyin-isearch; -*- ```

Or just use command: ``` M-x pinyin-isearch-forward ``` and ``` M-x pinyin-isearch-backward ```

C-u C-s for normal isearch
