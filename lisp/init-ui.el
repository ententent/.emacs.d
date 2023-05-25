;;; init-ui.el --- UI settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/lazycat-theme"))
(require 'lazycat-theme)
(lazycat-theme-load-dark)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/awesome-tray"))
(require 'awesome-tray)
(awesome-tray-mode 1)

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;    ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
;; 	doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-peacock t)
;;   (doom-themes-treemacs-config))

;; (load-theme 'modus-vivendi t)

;; sudo mv * /usr/share/fonts/truetype
;; sudo fc-cache -f -v
;; fc-list | grep Kaiti
;; https://einverne.github.io/post/2015/10/install-fonts-under-linux.html

;; 默认字体和字号 @ https://fonts.google.com/specimen/Fira+Mono
(set-face-attribute 'default nil :font "Fira Mono" :height 80)
;; 中文默认字体 @ https://mrswolf.github.io/my-manjaro-log/
(set-fontset-font "fontset-default" 'han "Kaiti")
;; 汉字间距显示问题
(setq inhibit-compacting-font-caches t)
;; 数学符号默认字体，保证 Unicode 数学符号可以正确显示
(set-fontset-font "fontset-default" 'mathematical "Cambria Math")
;; 固定间距字体 @ https://www.jetbrains.com/lp/mono/
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono"  :height 100)
;; 可变间距字体
(set-face-attribute 'variable-pitch nil :font "Segoe Print" :height 100 :weight 'regular)

;; 禁用一些GUI特性
(setq use-dialog-box nil)               ; 鼠标操作不使用对话框
(setq inhibit-default-init t)           ; 不加载 `default' 库
(setq inhibit-startup-screen t)         ; 不加载启动画面
(setq inhibit-startup-message t)        ; 不加载启动消息
(setq inhibit-startup-buffer-menu t)    ; 不显示缓冲区列表

;; 草稿缓冲区默认文字设置
(setq initial-scratch-message (concat ";; Happy hacking, "
                                      (capitalize user-login-name) " - Emacs ♥ you!\n\n"))

;; 设置缓冲区的文字方向为从左到右
(setq bidi-paragraph-direction 'left-to-right)
;; 禁止使用双向括号算法
;; (setq bidi-inhibit-bpa t)

;; 设置自动折行宽度为80个字符，默认值为70
(setq-default fill-column 80)

;; 设置大文件阈值为100MB，默认10MB
(setq large-file-warning-threshold 100000000)

;; 以16进制显示字节数
(setq display-raw-bytes-as-hex t)
;; 有输入时禁止 `fontification' 相关的函数钩子，能让滚动更顺滑
(setq redisplay-skip-fontification-on-input t)

;; 禁止响铃
(setq ring-bell-function 'ignore)

;; 禁止闪烁光标
(blink-cursor-mode -1)

;; 在光标处而非鼠标所在位置粘贴
(setq mouse-yank-at-point t)

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

;; yes或no提示设置，通过下面这个函数设置当缓冲区名字匹配到预设的字符串时自动回答yes
(setq original-y-or-n-p 'y-or-n-p)
(defalias 'original-y-or-n-p (symbol-function 'y-or-n-p))
(defun default-yes-sometimes (prompt)
  "automatically say y when buffer name match following string"
  (if (or
       (string-match "has a running process" prompt)
       (string-match "does not exist; create" prompt)
       (string-match "modified; kill anyway" prompt)
       (string-match "Delete buffer using" prompt)
       (string-match "Kill buffer of" prompt)
       (string-match "still connected.  Kill it?" prompt)
       (string-match "Shutdown the client's kernel" prompt)
       (string-match "kill them and exit anyway" prompt)
       (string-match "Revert buffer from file" prompt)
       (string-match "Kill Dired buffer of" prompt)
       (string-match "delete buffer using" prompt)
       (string-match "Kill all pass entry" prompt)
       (string-match "for all cursors" prompt)
       (string-match "Do you want edit the entry" prompt))
      t
    (original-y-or-n-p prompt)))
(defalias 'yes-or-no-p 'default-yes-sometimes)
(defalias 'y-or-n-p 'default-yes-sometimes)

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

;; 开启Emacs的视觉换行模式
(visual-line-mode 1)

;; 在模式栏上显示当前光标的列号
(column-number-mode t)

;; 开启行号后便于使用 M-g M-g 跳转到指定行
(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                eshell-mode-hook
                pdf-view-mode-hook
                eww-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
;;; highlight current line
(global-hl-line-mode t)

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
  :init
  (setq dashboard-banner-logo-title "Don't Panic, Take it Easy"
        dashboard-center-content t
        dashboard-items '((recents . 10)
                          (bookmarks . 5)
                          (projects . 5))
        dashboard-page-style 'truncate-middle
        dashboard-path-max-length 60
        dashboard-projects-backend 'projectile
        dashboard-set-footer t
        dashboard-set-init-info t
        dashboard-set-navigator t
        dashboard-show-shortcuts nil
        dashboard-startup-banner 'official)
  (dashboard-setup-startup-hook))

(provide 'init-ui)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
