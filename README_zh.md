![build](https://github.com/Anoncheg1/pinyin-isearch/workflows/melpazoid/badge.svg)
[![MELPA](https://melpa.org/packages/pinyin-isearch-badge.svg)](http://melpa.org/#/pinyin-isearch)
![build](https://github.com/Anoncheg1/pinyin-isearch/workflows/melpazoid-release/badge.svg)
[![MELPA Stable](https://stable.melpa.org/packages/pinyin-isearch-badge.svg)](https://stable.melpa.org/#/pinyin-isearch)

# pinyin-isearch - Emacs 插件：用不带声调的拼音在拼音和汉字中查找
允许在拼音文本中用不带声调的拼音进行查找。只输入几个字母就能找到全部的变体。

这是 Emacs Isearch 的“子模式”，它会替换 isearch-regexp-function，根据搜索内容自动生成正则表达式。

例如：你想找到 "Shànghǎi" 和 "上海"，只需要输入：``` C-s shanghai ```

基于 Emacs 的 "chinese-sisheng"、"chinese-py"、"chinese-punct"。

# 文件结构
```text
pinyin-isearch.el
 ├─ pinyin-isearch-pinyin.el (→ pinyin-isearch-loaders.el)
 ├─ pinyin-isearch-chars.el (→ pinyin-isearch-loaders.el)
 └─ pinyin-isearch-loaders.el
```

# 演示
![Demo](https://codeberg.org/Anoncheg/public-share/raw/branch/main/pinyin-isearch.gif)

# 特性
- 不会与其它 isearch 模式冲突
- 修复查找时跳跃但未返回的问题
- 默认支持用普通拉丁字母进行查找

## 汉字查找的功能
- 从第一个输入的字符开始查找
- 可以用英文符号查找中文标点：.,[]<>()$-"` 等等
- 准确拆分出所有可能的变体

## 拼音查找的功能
- 播音节之间的空格会自动忽略
- 首音节要求有声调或音标：例如 Zhēn de ma

# 安装方法
## 通过 MELPA 安装

1) 添加到 `~/.emacs`
```elisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
```

2) 用 `M-x package-install RET pinyin-isearch RET` 或 `M-x package-list-packages` 安装

## 通过 GitHub 或 Codeberg 安装
1) `git clone https://repo/user/pinyin-isearch`

2) 添加到 `~/.emacs`
```elisp
(add-to-list 'load-path "/path-to/pinyin-isearch/")
(require 'pinyin-isearch)
(pinyin-isearch-load) ;; （可选）提前加载
```

# 用法
在 isearch 模式下（按```C-s/r```后）：

- ```M-s h``` 只激活汉字查找子模式
- ```M-s p``` 只激活拼音查找子模式
- ```M-s s``` 激活严格拼音和汉字查找子模式
- ```M-s u``` 激活严格汉字查找子模式
- ```M-s n``` 激活默认 Pinyin-isearch 模式
- ```M-s r``` 激活标准查找模式

或者使用 M-x ```pinyin-isearch-forward/backward```

你可以设置某个文件默认使用此模式，在文件开头加：

```;-*- mode: pinyin-isearch; -*-```
# 设置
`M-x customize-group pinyin-isearch`

变量 `pinyin-isearch-default-mode` 用于设定 `C-s/r` 查找时的默认模式：只用拼音、只用汉字或严格模式等。