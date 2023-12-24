# pinyin-isearch
Emacs package with minor mode that allow search pinyin ignoring tones.

It replaces isearch-search-fun-function with our function that replace
 search string with regex that ignore tones.

For example, to find "Guānhuà" in text you just type: C-s guangua.
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
