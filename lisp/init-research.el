;;; init-research.el --- Research settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'cdlatex)

;; tex
;; @source https://github.com/jwiegley/use-package/issues/379#issuecomment-246161500
(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t))
;; 编译时问询主文件名称
(setq-default TeX-master nil)
;; 对新文件自动解析(usepackage, bibliograph, newtheorem等信息)
(setq TeX-parse-selt t)
;; PDF正向搜索相关设置
(setq TeX-PDF-mode t)
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-method 'synctex)
;; 使用pdf-tools打开PDF
(setq TeX-view-program-selection '((output-pdf "PDF Tools")))
;; 完成编译后刷新PDF文件
(add-hook 'TeX-after-compilation-finished-functions
	  #'TeX-revert-document-buffer)
;; 打开TeX文件时应执行的命令
(defun my-latex-hook ()
  (turn-on-cdlatex) ;; 加载cdlatex
  (turn-on-reftex) ;; 加载reftex
  (prettify-symbols-mode t) ;; 加载prettify-symbols-mode
  (outline-minor-mode) ;; 加载outline-mode
  (outline-hide-body)) ;; 打开文件时只显示章节标题
(add-hook 'LaTeX-mode-hook 'my-latex-hook)
;; prettify
;; 保证 Unicode 数学符号可以正确显示
(set-fontset-font "fontset-default" 'mathematical "Cambria Math")
;; 自动展开光标附近的宏命令
(setq prettify-symbols-unprettify-at-point t)
;; customize prettify
;; https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode
;; 可加入原列表中没有的编码、简化常用命令
(require 'tex-mode) ;; 载入 tex--prettify-symbols-alist 变量
(defun my/more-prettified-symbols ()
  (mapc (lambda (pair) (delete pair tex--prettify-symbols-alist))
        '(("\\supset" . 8835)))
  (mapc (lambda (pair) (cl-pushnew pair tex--prettify-symbols-alist))
        '(("\\Z" . 8484)
          ("\\Q" . 8474)
          ("\\N" . 8469)
          ("\\R" . 8477)
          ("\\eps" . 949)
          ("\\inf" . #x22C0) 
          ("\\sup". #x22C1)
          ("\\ONE" . #x1D7D9)
          ("\\mathbb{S}" . #x1D54A)
          ("\\PP" . #x2119)
          ("\\Ps" . #x1D5AF )
          ("\\Pp" . #x1D40F)
          ("\\E" . #x1D5A4)
          ("\\Ee" . #x1D404)
          ("\\EE" . #x1D53C )
          ("\\Fc" . #x2131)
          ("\\Nc" . #x1D4A9))))
(my/more-prettified-symbols) ;; 读入自定义 prettify 符号

(add-hook 'LaTeX-mode-hook (lambda()
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
  (setq TeX-save-query  nil )
  (setq TeX-show-compilation t)))

;; pdf-tools
;;; Windows Installation
;;;; M-x package-list-packages -> pdf-tools (from melpa-stable)
;;;; scoop install msys2
;;;; open mingw64.exe under msys2
;;;; pacman -S mingw-w64-x86_64-texlive-full
;;;; pacman -S mingw-w64-x86_64-emacs-pdf-toool-s-server
;;;; add E:\Scoop\apps\msys2\current\mingw64\bin to environment variable
(pdf-tools-install)
(define-key pdf-view-mode-map
  "d" 'pdf-view-next-page-command) ;; 向后翻页
(define-key pdf-view-mode-map
  "a" 'pdf-view-previous-page-command) ;; 向前翻页
(define-key pdf-view-mode-map
  "s" 'pdf-view-scroll-up-or-next-page) ;; 向下滑动
(define-key pdf-view-mode-map
  "w" 'pdf-view-scroll-down-or-previous-page) ;;向上滑动
(require 'pdf-annot)
(define-key pdf-annot-minor-mode-map (kbd "C-a a") 'pdf-annot-add-highlight-markup-annotation) ;; highlight
(define-key pdf-annot-minor-mode-map (kbd "C-a s") 'pdf-annot-add-squiggly-markup-annotation) ;; squiggly
(define-key pdf-annot-minor-mode-map (kbd "C-a u") 'pdf-annot-add-underline-markup-annotation) ;; underline
(define-key pdf-annot-minor-mode-map (kbd "C-a d") 'pdf-annot-delete) ;; delete
;; 禁用 =pdf-tools= 有关文件的本地化编译
(setq native-comp-deferred-compilation-deny-list '(".*pdf.*"))
;; 夜间模式设置绿色底色
(setq pdf-view-midnight-colors '("#000000" . "#9bCD9b"))
;; 默认夜间模式
(add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)
;; 打开PDF时自动缩放
(add-hook 'pdf-view-mode-hook 'pdf-view-fit-width-to-window)

;; 对于Windows系统，需要安装ImageMagick，并保证magick.exe在PATH变量的路径中
;;; 用msys2安装: pacman -S mingw-w64-x86_64-imagemagick
(use-package org-download
  :ensure async ;; 因为不是从melpa安装，需要手动保证async安装
  :defer t ;; 延迟加载
  :load-path "~/.emacs.d/lisp/"
  :bind
  (:map org-mode-map
        ("C-M-y" . org-download-clipboard)) ;; 绑定从剪贴板粘贴截图的快捷键
  :custom
  (org-download-heading-lvl 1) ;; 用一级标题给截图文件命名
  :config
  ;; 用同级 ./img 目录放置截图文件
  (setq-default org-download-image-dir "./img"))

(use-package org-noter
  :ensure t
  :custom
  (org-noter-notes-search-path '("~/org/notes/")) ;; 默认笔记路径。设置后从pdf文件中使用=org-noter=命令，会自动在该墓中寻找与文件同名的=.org=笔记文件
  (org-noter-auto-save-last-location t) ;; 自动保存上次阅读位置
  (org-noter-max-short-selected-text-length 20) ;; 修改长/短文本标准，默认为80
  (org-noter-default-heading-title "第 $p$ 页的笔记") ;; 默认短标题格式
  (org-noter-highlight-selected-text t) ;; 选中文字后插入笔记自动高亮
  :bind
  (("C-c n n" . org-noter) ;; 与org-roam配合，打开org-noter的快捷键
   :map org-noter-doc-mode-map ;; 加入左手键位
   ("e" . org-noter-insert-note)
   ("M-e" . org-noter-insert-precise-note)))

(provide 'init-research)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-research.el ends here
