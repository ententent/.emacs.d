;;; init-tools.el --- Tools settings -*- lexical-binding: t -*-
;;; Commentary: Useful tools to make Emacs efficient!

;;; Code:

(use-package exec-path-from-shell
  :load-path "~/.emacs.d/site-lisp/exec-path-from-shell/"
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package restart-emacs
  :load-path "~/.emacs.d/site-lisp/restart-emacs/")

(use-package which-key
  :load-path "~/.emacs.d/site-lisp/which-key/"
  :config
  (which-key-mode 1))

(use-package rime
  :load-path "~/.emacs.d/site-lisp/emacs-rime/"
  :config
  (setq rime-user-data-dir "~/.local/share/fcitx5/rime")
  (setq rime-posframe-properties
        (list :background-color "#333333"
              :foreground-color "#dcdccc"
              :font "Source Code Pro"
              :internal-border-width 10))
  (setq default-input-method "rime"
        rime-show-candidate 'posframe))

(use-package wraplish
  :load-path "~/.emacs.d/site-lisp/wraplish/"
  :hook
  ((markdown-mode org-mode) . wraplish-mode)
  :config
  (setq wraplish-add-space-after-chinese-punctuation t))

(provide 'init-tools)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tools.el ends here
