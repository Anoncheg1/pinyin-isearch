# pinyin-isearch
Emacs package with minor mode that allow search pinyin ignoring tones.

For example, to find "Guānhuà" in text you just type: C-s guangua.
# install
add to ~/.emacs

```elisp
(require 'pinyin-isearch) 
```

activate with M-x pinyin-isearch-mode or:
```elisp
(pinyin-isearch-mode)
```

or per file:
```  -*- mode: pinyin-isearch; -*- ```
