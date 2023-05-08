;;; init-completion.el --- Completion settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

  (use-package yasnippet
    :ensure t
    :diminish yas-minor-mode
    :hook ((after-init . yas-reload-all)
           ((prog-mode LaTeX-mode org-mode) . yas-minor-mode))
    :config
    (yas-reload-all)
    ;; add company-yasnippet to company-backends
    (defun company-mode/backend-with-yas (backend)
      (if (and (listp backend) (member 'company-yasnippet backend))
          backend
        (append (if (consp backend) backend (list backend))
                '(:with company-yasnippet))))
    (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
    ;; unbind <TAB> completion
    (define-key yas-minor-mode-map [(tab)]        nil)
    (define-key yas-minor-mode-map (kbd "TAB")    nil)
    (define-key yas-minor-mode-map (kbd "<tab>")  nil)
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
          (ignore-errors (yas-next-field)))))
    :bind
    (:map yas-minor-mode-map ("S-<tab>" . yas-expand)))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(global-set-key (kbd "M-/") 'hippie-expand)

(use-package orderless
  :defer 1
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; minibuffer helpful annotations
(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package all-the-icons-completion
  :ensure t
  :hook ((after-init . all-the-icons-completion-mode)
         (marginalia-mode . all-the-icons-completion-marginalia-setup))
  )

(use-package posframe
  :ensure t)

(provide 'init-completion)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-completion.el ends here
