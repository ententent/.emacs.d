;;; init-eaf.el --- emacs-application-framework settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
(require 'eaf)
;;; 配置 EAF 代理
(setq eaf-proxy-type "http")
(setq eaf-proxy-host "127.0.0.1")
(setq eaf-proxy-port "7890")

;; eaf-browser
(require 'eaf-browser)
(setq eaf-browser-continue-where-left-off t)
(setq eaf-browser-enable-adblocker t)
(setq browse-url-browser-function 'eaf-open-browser)

;; eaf-rss-reader
;;; 一些优质RSS订阅源
;;; https://manateelazycat.github.io/feed.xml          ;; manateeLazyCat
;;; https://superliooon.com/feed/                      ;; 即凉
;;; http://blindwith.science/index.xml                 ;; Blind with Science
;;; https://remacs.cc/index.xml                        ;; remacs的世界
;;; https://www.bmpi.dev/index.xml                     ;; BMPI
;;; http://www.ruanyifeng.com/blog/atom.xml            ;; Ruan YiFeng
;;; https://emacstalk.codeberg.page/podcast/index.xml  ;; EmacsTalk
;;; https://arxiv.org/rss/cs.CV                        ;; ArXiv CV
;;; https://sachachua.com/blog/feed/                   ;; Sacha Chua
;;; https://byvoid.com/zhs/feed.xml                    ;; byvoid
;;; https://www.skyue.com/feed                         ;; skyue
;;; https://einverne.github.io/feed.xml                ;; Verne
(require 'eaf-rss-reader)

;; eaf-git
;;; (require 'eaf-git)
;; eaf-pdf-viewer
;;; (require 'eaf-pdf-viewer)
;; eaf-image-viewer
(require 'eaf-image-viewer)
;; eaf-video-player
(require 'eaf-video-player)
;; eaf-music-player
(require 'eaf-music-player)

;; eaf-terminal
;;; (require 'eaf-terminal)
;; eaf-file-manager
(require 'eaf-file-manager)
;; eaf-system-monitor
(require 'eaf-system-monitor)
;; eaf-markdown-previewer
(require 'eaf-markdown-previewer)
;; eaf-org-previewer
(require 'eaf-org-previewer)

(provide 'init-eaf)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eaf.el ends here
