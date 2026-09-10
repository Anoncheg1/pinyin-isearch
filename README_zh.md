![build](https://github.com/Anoncheg1/pinyin-isearch/workflows/melpazoid/badge.svg)
[![MELPA](https://melpa.org/packages/pinyin-isearch-badge.svg)](http://melpa.org/#/pinyin-isearch)
![build](https://github.com/Anoncheg1/pinyin-isearch/workflows/melpazoid-release/badge.svg)
[![MELPA Stable](https://stable.melpa.org/packages/pinyin-isearch-badge.svg)](https://stable.melpa.org/#/pinyin-isearch)

# pinyin-isearch - 用于在拼音和汉字中进行无音调拼音搜索的 Emacs 包。

本包允许在拼音文本和汉字中使用不带变音符号的拼音进行搜索。创建了精确的正则表达式以匹配所有变体。
以 Emacs Isearch 修改的形式实现。
例如：要在文本中找到 "Shànghǎi" 和 "上海"，你只需输入：``` C-s shanghai ```。
基于 Emacs 的 "chinese-sisheng"、"chinese-py"、"chinese-punct"。
# 文件
```text
pinyin-isearch.el
 ├─ pinyin-isearch-pinyin.el (→ pinyin-isearch-loaders.el)
 ├─ pinyin-isearch-chars.el (→ pinyin-isearch-loaders.el)
 └─ pinyin-isearch-loaders.el
*-tests.el
```

# Emacs 版本支持
Emacs 28.1 -> 30.2

# 演示
![Demo](https://codeberg.org/Anoncheg/public-share/raw/branch/main/pinyin-isearch.gif)

# 特性
- 不会与其他 isearch 模式冲突
- 修复 isearch 跳跃但不返回的问题
- 默认回退到普通拉丁文本搜索
- 支持撇号（'’），如 "zú'ò" 足哦
- 无外部依赖

## 汉字搜索的特性
- 从输入的第一个字符开始搜索
- 使用 ASCII 字符搜索中文标点：.,[]<>()$-"` 以及更多
- 精确拆解为所有可能的变体

## 拼音搜索的特性
- 音节之间的空格被忽略
- 文本中仅第一个音节需要声调或变音符号：Zhēn de ma

# 安装
## 从 MELPA 安装

1) 添加到 `~/.emacs`
```elisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
```

2) 通过 `M-x package-install RET cui RET` 或 `M-x package-list-packages` 安装
## 从 GitHub 或 Codeberg 安装
1) `git clone https://repo/user/pinyin-isearch`

2) 添加到 `~/.emacs`
```elisp
(add-to-list 'load-path "/path-to/pinyin-isearch/")
(require 'pinyin-isearch)
(pinyin-isearch-load) ;; 在启用模式前强制加载（可选）
```

# 用法
**激活：** `M-x pinyin-isearch-mode`
**搜索：**
- `C-s` / `C-r` — 启动向前/向后搜索
- 输入拼音（例如 `beijing`）→ 高亮：北京, Běi jīng, beijing
- `C-n` / `C-p` — 导航匹配项
- `RET` — 确认，跳转到匹配项

换句话说：
1. M-x pinyin-isearch-mode [激活]
2. C-s [开始搜索]
3. "beijing" [输入拼音]
   → 高亮：北京 和 Běi jīng 和 beijing。
4. M-s h [切换到仅汉字]
   → 将结果过滤为仅精确汉字匹配
5. M-s s [启用严格模式]
   → 更精细的、仅精确匹配的结果
6. C-n / C-p [导航匹配项]
7. RET [确认，跳转到匹配项]

**直接函数：** `M-x pinyin-isearch-forward/backward`
**子模式控制（搜索期间）：**
| 按键 | 效果 |
|---------|--------|
| `M-s h` | 仅汉字搜索 |
| `M-s p` | 仅拼音搜索 |
| `M-s b` | 拼音和汉字（默认） |
| `M-s s` | 切换严格模式（仅精确匹配） |
| `M-s <f1>` | 帮助参考 |

**回退：** 随时 `C-u C-s` → 标准 Emacs isearch（完全绕过拼音）
对于**文件局部**激活，在文件开头添加此行：
```elisp
;-*- mode: pinyin-isearch; -*-
```

# 配置
`M-x customize-group pinyin-isearch`
| 选项 | 默认值 | 效果 |
|--------|---------|--------|
| `pinyin-isearch-default-mode` | `both` | 模式启动时的默认搜索类型 |
| `pinyin-isearch-strict` | `nil` | 全局严格性设置 |
| `pinyin-isearch-full-fallback` | `t` | 包含拉丁字母回退 |
| `pinyin-isearch-fix-jumping-flag` | `t` | 修复搜索重启位置行为 |

# 其他包
- Dired、Packages、Buffers 模式中的导航 https://github.com/Anoncheg1/firstly-search
- 用于 Org-mode 的 LLM 聊天块 https://github.com/Anoncheg1/emacs-cui
- Ediff 修复 https://github.com/Anoncheg1/ediffnw
- Dired 历史记录 https://github.com/Anoncheg1/dired-hist
- 选中窗口对比度 https://github.com/Anoncheg1/selected-window-contrast
- 复制链接到剪贴板 https://github.com/Anoncheg1/emacs-org-links
- "回调地狱"的解决方案 https://github.com/Anoncheg1/emacs-async1
- 恢复缓冲区状态 https://github.com/Anoncheg1/emacs-unmodified-buffer1
- outline.el 用法 https://github.com/Anoncheg1/emacs-outline-it
- 在咖啡馆隐藏密码 https://github.com/Anoncheg1/emacs-hidepass
- TAB 键重新实现 https://github.com/Anoncheg1/emacs-indent
- Org-mode 标题的日期 https://github.com/Anoncheg1/emacs-org-history

# 捐赠，赞助作者
您可以直接使用加密货币赞助作者：
- **BTC (Bitcoin) 地址：** `1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25`

![](https://raw.githubusercontent.com/Anoncheg1/public-share/refs/heads/main/BTC-1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25.png)
- **USDT (Tether on TRX-TRON) 地址：** `TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN`
![](https://raw.githubusercontent.com/Anoncheg1/public-share/refs/heads/main/USDT-TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN.png)
- **TON (Telegram Open Network) 地址：** `UQC8rjJFCHQkfdp7KmCkTZCb5dGzLFYe2TzsiZpfsnyTFt9D`
