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

(defun my/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (delete-word (- arg))))

(use-package vertico
  :defer 1
  :custom
  (verticle-cycle t)
  :config
  (vertico-mode)
  :bind (:map minibuffer-local-map
              ("M-h" .  my/minibuffer-backward-kill)))

;; minibuffer helpful annotations
(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package orderless
  :defer 1
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package all-the-icons-completion
  :ensure t
  :hook ((after-init . all-the-icons-completion-mode)
         (marginalia-mode . all-the-icons-completion-marginalia-setup))
  )

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
