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
(require 'tex-mode)
(defun my/more-prettified-symbols ()
  (mapc (lambda (pair) (cl-pushnew pair tex--prettify-symbols-alist))
        '(("\\Z" . 8484) ;; 大多数人在latex中会用 \Z, \Q, \N, \R 表示数域
          ("\\Q" . 8474)
          ("\\N" . 8469)
          ("\\R" . 8477)
          ("\\eps" . 949)
          ("\\ONE" . #x1D7D9)
          ("\\mathbb{S}" . #x1D54A)
          ("\\PP" . #x2119) ;; 个人需要, 经常要使用P和E的数学字体
          ("\\P" . #x1D5AF )
          ("\\Pp" . #x1D40F)
          ("\\E" . #x1D5A4)
          ("\\Ee" . #x1D404)
          ("\\EE" . #x1D53C )
          ("\\Fc" . #x2131)
          ("\\Nc" . #x1D4A9))))
(my/more-prettified-symbols)

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
;; 打开PDF时自动缩放
(add-hook 'pdf-view-mode-hook 'pdf-view-fit-width-to-window)

(use-package org-download
  :ensure async ;; 因为不是从melpa安装org-download，需要手动保证async安装
  :defer t ;; 延迟加载
  :load-path "~/.emacs.d/lisp/"
  :bind
  (:map org-mode-map
        ("C-M-y" . org-download-clipboard)) ;; 绑定从剪贴板粘贴截图的快捷键
  :custom
  (org-download-heading-lvl 1) ;; 用一级标题给截图文件命名
  :config
  (setq-default org-download-image-dir "./img")) ;; 用同级 ./img 目录放置截图文件

(add-hook 'org-mode-hook #'org-cdlatex-mode) ;; 打开 cdlatex
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

(use-package org-noter
  :ensure t)

(provide 'init-research)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-research.el ends here
