# pinyin-isearch
Emacs package that allow search pinyin, ignoring tones.

Allow to search with pinyin in pinyin text and ignore tone diacritical marks for speed.

Isearch "submode" that replace isearch-regexp-function to generate regex.

For example, to find "Shànghǎi" in text you just type: C-s shanghai.

# Features
- white spaces are ignored between syllables
- tone required only in first syllable in text: Zhēn de ma
- should not conflict with other isearch modes
- fix isearch behavior when new search begins from last successed occurance and jump down


# Installation
copy file to ~/.emacs.d/contrib/pinyin-isearch.el

add to ~/.emacs

```elisp
(add-to-list 'load-path "~/.emacs.d/contrib/")
(require 'pinyin-isearch)
```

# Usage
After ```C-s/r``` in isearch mode: ```M-s n``` to activate isearch submode.

or with M-x ```pinyin-isearch-forward/backward```

You can set this mode by default per file with:

```;-*- mode: pinyin-isearch; -*-```

# Configuration
M-x customize-group pinyin-isearch
