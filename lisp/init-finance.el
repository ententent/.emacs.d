;;; init-finance.el --- Editing settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.emacs.d/site-lisp/beancount/")
(require 'beancount)
(add-to-list 'auto-mode-alist '("\\.bean\\'" . beancount-mode))
;; (add-hook 'beancount-mode-hook
;;   (lambda () (setq-local electric-indent-chars nil)))
(add-hook 'beancount-mode-hook #'outline-minor-mode)
(define-key beancount-mode-map (kbd "C-c C-n") #'outline-next-visible-heading)
(define-key beancount-mode-map (kbd "C-c C-p") #'outline-previous-visible-heading)

;; (message "init-finance configuration: %.2fs"
;;          (float-time (time-subtract (current-time) my/init-base-start-time)))

(provide 'init-finance)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
