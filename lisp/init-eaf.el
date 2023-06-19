;;; init-eaf.el --- emacs-application-framework settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

  (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
  (require 'eaf)

  ;;; eaf general settings
  (setq eaf-python-command "/usr/bin/python")

  ;;; eaf-browser
  (require 'eaf-browser)
  (setq eaf-browser-continue-where-left-off t)
  (setq eaf-browser-enable-adblocker t)
  (setq browse-url-browser-function 'eaf-open-browser)

  ;;; eaf-rss-reader
  ;; 一些优质RSS订阅源
  ;; https://manateelazycat.github.io/feed.xml           manateeLazyCat
  ;; https://superliooon.com/feed/                       即凉
  ;; http://blindwith.science/index.xml                  Blind with Science
  ;; https://remacs.cc/index.xml                         remacs的世界
  ;; https://www.bmpi.dev/index.xml                      BMPI
  ;; http://www.ruanyifeng.com/blog/atom.xml             Ruan YiFeng
  ;; https://emacstalk.codeberg.page/podcast/index.xml   EmacsTalk
  ;; https://arxiv.org/rss/cs.CV                         ArXiv CV
  ;; https://sachachua.com/blog/feed/                    Sacha Chua
  ;; https://byvoid.com/zhs/feed.xml                     byvoid
  ;; https://www.skyue.com/feed                          skyue
  ;; https://einverne.github.io/feed.xml                 Verne
  ;; https://liujiacai.net/atom.xml                      刘家财
  ;; https://planet.emacslife.com/                       Alvaro Ramirez
  ;; https://leovan.me/index.xml                         范叶亮
  (require 'eaf-rss-reader)

  ;;; eaf-pdf-viewer
  (require 'eaf-pdf-viewer)
  ;; 配置 eaf-interleave, 配合使用 eaf-pdf-viewer 和 org-mode
  ;; https://github.com/emacs-eaf/emacs-application-framework/blob/master/extension/eaf-interleave.el
  (require 'eaf-interleave)
  (add-hook 'eaf-browser-hook 'eaf-interleave-app-mode)
  (add-hook 'eaf-pdf-viewer-hook 'eaf-interleave-app-mode)
  (add-hook 'org-mode-hook 'eaf-interleave-mode)
  (with-eval-after-load 'eaf-interleave
    (define-key eaf-interleave-mode-map (kbd "M-.") 'eaf-interleave-sync-current-note)
    (define-key eaf-interleave-mode-map (kbd "M-p") 'eaf-interleave-sync-previous-note)
    (define-key eaf-interleave-mode-map (kbd "M-n") 'eaf-interleave-sync-next-note)
    (define-key eaf-interleave-app-mode-map (kbd "C-c M-o") 'eaf-interleave-open-notes-file)
    (define-key eaf-interleave-app-mode-map (kbd "C-c M-i") 'eaf-interleave-add-note)    
    (define-key eaf-interleave-app-mode-map (kbd "C-c M-q") 'eaf-interleave-quit))
  ;; 默认笔记路径, 自动在该目录中寻找同名 org 文件
  (setq eaf-interleave-org-notes-dir-list '("~/org/roam/interleave"))
  ;; 分隔文档和笔记
  (setq eaf-interleave-split-direction 'vertical
	eaf-interleave-split-lines 20
	eaf-interleave-disable-narrowing t)

  ;;; eaf-image-viewer
  (require 'eaf-image-viewer)

  ;;; eaf-js-video-player
  (require 'eaf-js-video-player)

  ;;; eaf-music-player
  (require 'eaf-music-player)
  (setq eaf-music-default-file "~/MEGA/Music/songs")

  ;;; eaf-pyqterminal
  (require 'eaf-pyqterminal)
  (setq eaf-pyqterminal-font-family "JetBrainsMono Nerd Font")

  ;;; eaf-file-manager
  (require 'eaf-file-manager)

  ;;; eaf-markdown-previewer
  (require 'eaf-markdown-previewer)

  ;;; eaf-org-previewer
  (require 'eaf-org-previewer)

(provide 'init-eaf)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eaf.el ends here
