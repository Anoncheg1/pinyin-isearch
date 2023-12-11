# pinyin-isearch
Emacs package with minor mode that allow search pinyin ignoring tones.

For example, to find "Guānhuà" in text you just type: C-s guangua.
# install
copy file to ~/.emacs.d/contrib/pinyin-isearch.el

add to ~/.emacs

```elisp
(add-to-list 'load-path "~/.emacs.d/contrib/")
(require 'pinyin-isearch) 
```

activate with M-x pinyin-isearch-mode or:
```elisp
(pinyin-isearch-mode)
```

or per file:
```  -*- mode: pinyin-isearch; -*- ```
