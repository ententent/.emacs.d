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

(provide 'init-eaf)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eaf.el ends here
