;;; init-english.el --- English Tools settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/site-lisp/sdcv/")
(require 'sdcv)

(use-package sdcv
  :commands (sdcv-search-pointer+)
  :bind
  (("\e\e ," . sdcv-search-pointer+)
   ("\e\e ." . sdcv-search-pointer))
  :config
  (setq sdcv-say-word-p t)
  (setq sdcv-dictionary-data-dir (expand-file-name "~/.emacs.d/site-lisp/sdcv"))
  (setq sdcv-dictionary-simple-list
      '(
        "懒虫简明英汉词典"
        "懒虫简明汉英词典"
        ))
  (setq sdcv-dictionary-complete-list
      '(
        "朗道英汉字典5.0"
        "牛津英汉双解美化版"
        ))
  (setq sdcv-tooltip-timeout 10)
  (setq sdcv-fail-notify-string "没找到释义")
  (setq sdcv-tooltip-border-width 2)
  )

(add-to-list 'load-path "~/.emacs.d/site-lisp/popweb/")
(setq popweb-python-command "/usr/bin/python")
;; Proxy
(setq popweb-proxy-type "http")
(setq popweb-proxy-host "127.0.0.1")
(setq popweb-proxy-port "7890")
;; Font size
(setq popweb-zoom-factor 0.5)
;; Scaled to emacs
(setq popweb-url-web-window-width-scale 0.8)
(setq popweb-url-web-window-height-scale 0.15)

;; LaTeX preview functionality
(add-to-list 'load-path "~/.emacs.d/site-lisp/popweb/extension/latex")
(require 'popweb-latex)
(add-hook 'latex-mode-hook #'popweb-latex-mode)

;; Chinese-English translation popup
(add-to-list 'load-path "~/.emacs.d/site-lisp/popweb/extension/dict") ;
(require 'popweb-dict)

(provide 'init-english)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-english.el ends here
