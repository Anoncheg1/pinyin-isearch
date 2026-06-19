![build](https://github.com/Anoncheg1/pinyin-isearch/workflows/melpazoid/badge.svg)
[![MELPA](https://melpa.org/packages/pinyin-isearch-badge.svg)](http://melpa.org/#/pinyin-isearch)
![build](https://github.com/Anoncheg1/pinyin-isearch/workflows/melpazoid-release/badge.svg)
[![MELPA Stable](https://stable.melpa.org/packages/pinyin-isearch-badge.svg)](https://stable.melpa.org/#/pinyin-isearch)

# pinyin-isearch - Emacs 里可以用拼音（不带声调）或中文字来查找的插件

可以用没有声调的拼音来查找拼音或中文。只用很少的字母就能找到所有的写法。
这个“小功能”会换掉 isearch 里的正则表达式（regexp）生成方法，让你可以更好地查找。
比如：你想找到 "Shànghǎi" 和 "上海"，只需要输入：``` C-s shanghai ```
# 文件
```text
pinyin-isearch.el
 ├─ pinyin-isearch-pinyin.el (→ pinyin-isearch-loaders.el)
 ├─ pinyin-isearch-chars.el (→ pinyin-isearch-loaders.el)
 └─ pinyin-isearch-loaders.el
```

# 演示
![Demo](https://codeberg.org/Anoncheg/public-share/raw/branch/main/pinyin-isearch.gif)

# 特点
- 不会和其它查找功能冲突
- 修正查找时跳来跳去的问题
- 如果不开严格模式，还可以用正常英文字母查找

## 查找汉字的特点
- 从第一个输入的字就开始查找
- 用英文符号查找中文里的标点：.,[]<>()$-"` 等等
- 分析所有可能的写法，准确查找到

## 用拼音查找的特点
- 拼音字母之间的空格会自动忽略
- 只有第一个音节需要加声调：比如 Zhēn de ma

# 安装
## 在 MELPA 上安装

1) 加到 `~/.emacs`
```elisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
```

2) 用 `M-x package-install RET cui RET~ 或 ~M-x package-list-packages` 安装
## 用 GitHub 或 Codeberg 安装
1) `git clone https://repo/user/pinyin-isearch`

2) 在 `~/.emacs` 里加入
```elisp
(add-to-list 'load-path "/path-to/pinyin-isearch/")
(require 'pinyin-isearch)
(pinyin-isearch--load) ;; （可选）提前加载
```

# 用法
在 isearch 查找模式时，按 ```C-s/r``` 后：
- ```M-s p``` 开启拼音查找
- ```M-s h``` 开启汉字查找
- ```M-s s``` 开启严格汉字查找
- ```M-s r``` 回到普通查找

或者用 M-x ```pinyin-isearch-forward/backward```
你也可以让每个文件默认用这个查找模式，加：
```;-*- mode: pinyin-isearch; -*-```
# 设置
输入 `M-x customize-group pinyin-isearch`
# 计划
- 拼音支持大写
- 支持仓颉查找