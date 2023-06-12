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

(provide 'init-tools)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tools.el ends here
