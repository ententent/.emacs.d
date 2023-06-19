;;; init-ai.el --- AI Tools settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/site-lisp/mind-wave/")
(require 'mind-wave)
(setq mind-wave-auto-change-title nil) ; 避免与auto-save插件的冲突

(use-package org-ai
  :ensure t
  :bind (("C-c q" . org-ai-prompt)
         ("C-c x" . org-ai-on-region))
  :hook (org-mode . org-ai-mode)
  :config
  (setq org-ai-default-max-tokens 480)
  (setq org-ai-default-chat-system-prompt "你是一个 Emacs 助手，请以 Org-mode 的格式来回复我"))

(provide 'init-ai)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
