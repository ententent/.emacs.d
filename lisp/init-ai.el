;;; init-ai.el --- AI Tools settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/site-lisp/mind-wave/")
(require 'mind-wave)
(setq mind-wave-auto-change-title nil) ; 避免与auto-save插件的冲突

(provide 'init-ai)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
