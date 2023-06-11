;;; init-ui.el --- UI settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/lazycat-theme"))
(require 'lazycat-theme)
(lazycat-theme-load-dark)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/awesome-tray"))
(require 'awesome-tray)
(awesome-tray-mode 1)

;; sudo mv * /usr/share/fonts/truetype
;; sudo fc-cache -f -v
;; fc-list | grep Kaiti
;; https://einverne.github.io/post/2015/10/install-fonts-under-linux.html

;; 默认字体和字号 @ https://fonts.google.com/specimen/Fira+Mono
(set-face-attribute 'default nil :font "Fira Mono" :height 135)
;; 中文默认字体 @ https://mrswolf.github.io/my-manjaro-log/
(set-fontset-font "fontset-default" 'han "Kaiti")
;; 汉字间距显示问题
(setq inhibit-compacting-font-caches t)
;; 数学符号默认字体，保证 Unicode 数学符号可以正确显示
(set-fontset-font "fontset-default" 'mathematical "Cambria Math")
;; 固定间距字体 @ https://www.jetbrains.com/lp/mono/
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono"  :height 135)
;; 可变间距字体
(set-face-attribute 'variable-pitch nil :font "Segoe Print" :height 135 :weight 'regular)

  (fset 'yes-or-no-p 'y-or-n-p)                   ; 以 y/n 代表 yes/no
  (blink-cursor-mode -1)                          ; 禁止闪烁光标
  (transient-mark-mode 1)                         ; 标记高亮
  (global-subword-mode 1)                         ; Word 移动支持 FooBar 的格式

  (setq confirm-kill-processes nil)               ; 退出自动杀掉进程
  (setq initial-scratch-message "")               ; 关闭启动空白buffer, 避免干扰session恢复
  (setq ring-bell-function 'ignore)               ; 关闭出错时的提示音
  (setq mouse-yank-at-point t)                    ; 在光标处而非鼠标所在位置粘贴
  
  ;; 禁用一些GUI特性
  (setq use-dialog-box nil)                       ; 鼠标操作不使用对话框
  (setq inhibit-default-init t)                   ; 不加载 `default' 库
  (setq inhibit-startup-screen t)                 ; 不加载启动画面
  (setq inhibit-startup-message t)                ; 不加载启动消息
  (setq inhibit-startup-buffer-menu t)            ; 不显示缓冲区列表

  ;; 增加长行处理性能
  (setq bidi-inhibit-bpa t)
  (setq-default bidi-paragraph-direction 'left-to-right)

  ;; 设置自动折行宽度为80个字符，默认值为70
  (setq-default fill-column 80)

  ;; 设置大文件阈值为100MB，默认10MB
  (setq large-file-warning-threshold 100000000)

  ;; 以16进制显示字节数
  (setq display-raw-bytes-as-hex t)
  ;; 有输入时禁止 `fontification' 相关的函数钩子，能让滚动更顺滑
  (setq redisplay-skip-fontification-on-input t)

  ;; 拷贝粘贴设置
  (setq select-enable-primary nil)        ; 选择文字时不拷贝
  (setq select-enable-clipboard t)        ; 拷贝时使用剪贴板

  ;; 鼠标滚动设置
  (setq scroll-step 2)
  (setq scroll-margin 2)
  (setq hscroll-step 2)
  (setq hscroll-margin 2)
  (setq scroll-conservatively 101)
  (setq scroll-up-aggressively 0.01)
  (setq scroll-down-aggressively 0.01)
  (setq scroll-preserve-screen-position 'always)

  ;; 对于高的行禁止自动垂直滚动
  (setq auto-window-vscroll nil)

  ;; 设置新分屏打开的位置的阈值
  (setq split-width-threshold (assoc-default 'width default-frame-alist))
  (setq split-height-threshold nil)

  ;; TAB键设置，在Emacs里不使用TAB键，所有的TAB默认为4个空格
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)

  ;; 设置剪贴板历史长度300，默认为60
  (setq kill-ring-max 200)

  ;; 在剪贴板里不存储重复内容
  (setq kill-do-not-save-duplicates t)

  ;; 设置位置记录长度为6，默认为16
  ;; 可以使用 `counsel-mark-ring' or `consult-mark' (C-x j) 来访问光标位置记录
  ;; 使用 C-x C-SPC 执行 `pop-global-mark' 直接跳转到上一个全局位置处
  ;; 使用 C-u C-SPC 跳转到本地位置处
  (setq mark-ring-max 6)
  (setq global-mark-ring-max 6)

  ;; 设置 emacs-lisp 的限制
  (setq max-lisp-eval-depth 10000)        ; 默认值为 800
  (setq max-specpdl-size 10000)           ; 默认值为 1600

  ;; 启用 `list-timers', `list-threads' 这两个命令
  (put 'list-timers 'disabled nil)
  (put 'list-threads 'disabled nil)

  ;; 在命令行里支持鼠标
  (xterm-mouse-mode 1)

  ;; 退出Emacs时进行确认
  (setq confirm-kill-emacs 'y-or-n-p)

  (global-display-line-numbers-mode t)    ; 开启行号后便于使用 M-g M-g 跳转
  (column-number-mode t)                  ; 在模式栏上显示当前光标的列号
  (global-hl-line-mode t)                 ; 高亮当前行
  (visual-line-mode 1)                    ; 视觉换行模式
  ; Line numbers are not displayed when large files are used.
  (setq line-number-display-limit large-file-warning-threshold)
  (setq line-number-display-limit-width 1000)
  
  (dolist (hook (list
               'c-mode-common-hook
               'c-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'java-mode-hook
               'asm-mode-hook
               'haskell-mode-hook
               'rcirc-mode-hook
               'erc-mode-hook
               'sh-mode-hook
               'makefile-gmake-mode-hook
               'python-mode-hook
               'js-mode-hook
               'html-mode-hook
               'css-mode-hook
               'tuareg-mode-hook
               'go-mode-hook
               'coffee-mode-hook
               'qml-mode-hook
               'markdown-mode-hook
               'slime-repl-mode-hook
               'package-menu-mode-hook
               'cmake-mode-hook
               'php-mode-hook
               'web-mode-hook
               'coffee-mode-hook
               'sws-mode-hook
               'jade-mode-hook
               'vala-mode-hook
               'rust-mode-hook
               'ruby-mode-hook
               'qmake-mode-hook
               'lua-mode-hook
               'swift-mode-hook
               'llvm-mode-hook
               'conf-toml-mode-hook
               'nxml-mode-hook
               'nim-mode-hook
               'typescript-mode-hook
               'elixir-mode-hook
               'clojure-mode-hook
               'dart-mode-hook

               'c-ts-mode-hook
               'c++-ts-mode-hook
               'cmake-ts-mode-hook
               'toml-ts-mode-hook
               'css-ts-mode-hook
               'js-ts-mode-hook
               'json-ts-mode-hook
               'python-ts-mode-hook
               'bash-ts-mode-hook
               'typescript-ts-mode-hook
               'rust-ts-mode-hook
               ))
  (add-hook hook (lambda () (display-line-numbers-mode))))

  ;; Disable line numbers for some modes
  (dolist (mode '(term-mode-hook
                  eshell-mode-hook
                  pdf-view-mode-hook
                  eww-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; 配置所有的编码为UTF-8，参考：
;; https://thraxys.wordpress.com/2016/01/13/utf-8-in-emacs-everywhere-forever/
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs!") ;; 个性签名，随读者喜好设置
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-startup-banner 'official)
  (setq dashboard-items '((recents  . 5)
			              (bookmarks . 5)
			              (projects . 10)))
  (dashboard-setup-startup-hook))

(provide 'init-ui)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
