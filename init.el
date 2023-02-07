;; mirror
;; @source https://mirrors.tuna.tsinghua.edu.cn/help/elpa/
(require 'package)
(setq package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; use-package
;; M-x package-install RET use-package
(eval-when-compile
  (require 'use-package))

;; doom-themes
;; @source https://github.com/doomemacs/themes#manually--use-package
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-acario-dark t)
  (doom-themes-treemacs-config))

;; all-the-icons
;; @source https://github.com/domtronn/all-the-icons.el#installation
;; On Windows, after downloading the fonts, one needs to manually install them
;; M-x package-install RET all-the-icons
;; restart-emacs, M-x all-the-icons-install-fonts
(when (display-graphic-p)
  (require 'all-the-icons))

;; treemacs for workspace management
;; depend on all-the-icons
(use-package treemacs
  :ensure t
  :defer t
  :config
  (treemacs-tag-follow-mode)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ;; ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  (:map treemacs-mode-map
	("/" . treemacs-advanced-helpful-hydra)))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package lsp-treemacs
  :ensure t
  :after (treemacs lsp))

;; cnfonts
(use-package cnfonts
  :ensure t
  :config
  (cnfonts-mode 1))

;; tex
;; @source https://github.com/jwiegley/use-package/issues/379#issuecomment-246161500
(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(exec-path-from-shell auctex cnfonts all-the-icons lsp-treemacs treemacs-projectile treemacs doom-themes use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
