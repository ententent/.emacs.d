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

;; (require 'eaf-pdf-viewer)

(require 'eaf-rss-reader)
;; 一些优质RSS订阅源
;; http://blindwith.science/index.xml                 ;; Blind with Science
;; https://remacs.cc/index.xml                        ;; remacs的世界
;; https://manateelazycat.github.io/feed.xml          ;; manateeLazyCat
;; http://feeds.feedburner.com/ruanyifeng             ;; Ruan YiFeng
;; https://emacstalk.codeberg.page/podcast/index.xml  ;; EmacsTalk
;; https://arxiv.org/rss/cs.CV                        ;; ArXiv CV
;; https://superlioon.com/feed/                       ;; 即凉
;; https://sachachua.com/blog/feed/                   ;; Sacha Chua

(provide 'init-eaf)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eaf.el ends here
