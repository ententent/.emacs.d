;;; init-english.el --- English Tools settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/site-lisp/sdcv/")
(require 'sdcv)

(use-package sdcv
  :commands (sdcv-search-pointer+)
  :bind
  (("\e\e w ," . sdcv-search-pointer+)
   ("\e\e w ." . sdcv-search-pointer))
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

(provide 'init-english)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-english.el ends here
