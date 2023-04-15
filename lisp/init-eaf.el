;;; init-eaf.el --- emacs-application-framework settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
(require 'eaf)
;;; 配置 EAF 代理
(setq eaf-proxy-type "http")
(setq eaf-proxy-host "127.0.0.1")
(setq eaf-proxy-port "7890")

(require 'eaf-browser)
(setq eaf-browser-continue-where-left-off t)
(setq eaf-browser-enable-adblocker t)
(setq browse-url-browser-function 'eaf-open-browser)

(require 'eaf-rss-reader)
;; 一些优质RSS订阅源
;; https://manateelazycat.github.io/feed.xml          ;; manateeLazyCat
;; https://superlioon.com/feed/                       ;; 即凉
;; http://blindwith.science/index.xml                 ;; Blind with Science
;; https://remacs.cc/index.xml                        ;; remacs的世界
;; https://www.bmpi.dev/index.xml                     ;; BMPI
;; http://feeds.feedburner.com/ruanyifeng             ;; Ruan YiFeng
;; https://emacstalk.codeberg.page/podcast/index.xml  ;; EmacsTalk
;; https://arxiv.org/rss/cs.CV                        ;; ArXiv CV
;; https://sachachua.com/blog/feed/                   ;; Sacha Chua

(require 'eaf-git)

(require 'eaf-terminal)

(require 'eaf-system-monitor)

(require 'eaf-file-manager)

(require 'eaf-markdown-previewer)

(require 'eaf-org-previewer)

;; (require 'eaf-pdf-viewer)

(require 'eaf-image-viewer)

(require 'eaf-music-player)

(require 'eaf-video-player)

(provide 'init-eaf)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eaf.el ends here