;;; init-completion.el --- Completion settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

  (use-package yasnippet
    :ensure t
    :diminish yas-minor-mode
    :hook ((after-init . yas-reload-all)
           ((prog-mode LaTeX-mode org-mode) . yas-minor-mode))
    :config
    ;; Suppress warning for yasnippet code.
    (require 'warnings)
    (yas-global-mode 1)
    (add-to-list 'warning-suppress-types '(yasnippet backquote-change))

    (setq yas-prompt-functions '(yas-x-prompt yas-dropdown-prompt))
    (defun smarter-yas-expand-next-field ()
      "Try to `yas-expand' then `yas-next-field' at current cursor position."
      (interactive)
      (let ((old-point (point))
            (old-tick (buffer-chars-modified-tick)))
        (yas-expand)
        (when (and (eq old-point (point))
                   (eq old-tick (buffer-chars-modified-tick)))
          (ignore-errors (yas-next-field))))))

;; minibuffer helpful annotations
(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package posframe
  :ensure t)

;;lsp-bridge
;;; https://github.com/manateelazycat/lsp-bridge/blob/master/README.zh-CN.md
(add-to-list 'load-path "~/.emacs.d/lsp-bridge/")
(require 'lsp-bridge)
(global-lsp-bridge-mode)
(setq lsp-bridge-python-lsp-server "pyright")

(provide 'init-completion)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-completion.el ends here
