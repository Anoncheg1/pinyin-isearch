![example workflow](https://github.com/Anoncheg1/pinyin-isearch/actions/workflows/test.yml/badge.svg?event=release)

Tested with Emacs 29.3

# pinyin-isearch
Emacs package for toneless pinyin search in pinyin and Chinese characters.

Allow to search with pinyin without diacritical marks in pinyin text. With few characters you will find all variants.

Emacs Isearch "submode" that replace isearch-regexp-function to generate regex for search.

For example: to find "Shànghǎi" and "上海" in text you just type: ``` C-s shanghai ```.

# Demonstation
![Demo](https://codeberg.org/Anoncheg/public-share/raw/branch/main/pinyin-isearch.gif)

# Featurs
- should not conflict with other isearch modes
- fix isearch jumping without return.
- fallback to search for normal latin text if strict mode is not enabled.

## Features of Chinese characters search
- search from the first character entered.
- punctulation Chinese search with ascii charactes: .,[]<>()$-"` and much more.
- accurate dissasemble to all possible variants.

## Features of pinyin search
- white spaces are ignored between syllables
- tone or diacritical mark required only in first syllable in text: Zhēn de ma

# Installation
copy file to ~/.emacs.d/contrib/pinyin-isearch.el

add to ~/.emacs

```elisp
(add-to-list 'load-path "~/.emacs.d/contrib/")
(require 'pinyin-isearch)
```

# Usage
After ```C-s/r``` in isearch mode:
- ```M-s p``` to activate pinyin isearch submode.
- ```M-s h``` to activate Chinese characters isearch submode.
- ```M-s s``` to activate strict Chinese characters isearch submode.
- ```M-s r``` to activate standard search.

or with M-x ```pinyin-isearch-forward/backward```

You can set this mode by default per file with:

```;-*- mode: pinyin-isearch; -*-```

# Configuration
M-x customize-group pinyin-isearch

# Todo
- Upperacase for pinyin.
- Cangjie search
