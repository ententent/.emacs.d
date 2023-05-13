;;; init-lsp.el --- Completion settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package posframe
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

(add-to-list 'load-path "~/.emacs.d/lsp-bridge/")
(require 'lsp-bridge)
(global-lsp-bridge-mode)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'popon)

(unless (package-installed-p 'yasnippet)
  (package-install 'yasnippet))

(unless (display-graphic-p)
  (add-to-list 'load-path "~/.emacs.d/lisp/"))

(provide 'init-lsp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
