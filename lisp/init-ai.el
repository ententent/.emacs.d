;;; init-ai.el --- AI Tools settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/site-lisp/mind-wave/")
(require 'mind-wave)
(setq mind-wave-auto-change-title nil) ; 避免与auto-save插件的冲突

(add-to-list 'load-path "~/.emacs.d/site-lisp/org-ai/")
(require 'org-ai)
(add-hook 'org-mode-hook #'org-ai-mode)
(org-ai-global-mode)
(setq org-ai-default-chat-system-prompt "你是一个 Emacs 助手，请以 Org-mode 的格式来回复我")

(provide 'init-ai)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
