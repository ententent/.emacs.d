;;; init.el --- The main init entry for Emacs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(require 'package)
(setq package-check-signature nil)
(setq package-archives
	  '(("gnu"          . "http://1.15.88.122/gnu/")
	    ("melpa"        . "http://1.15.88.122/melpa/")
        ("melpa-stable" . "http://1.15.88.122/stable-melpa/")
	    ("nongnu"       . "http://1.15.88.122/nongnu/")))
(package-initialize)

;; 安装 use-package
(unless (package-installed-p 'use-package)
   (package-refresh-contents)
   (package-install 'use-package))

;; 配置 use-package
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t)
  (if (daemonp)
      (setq use-package-always-demand t)))

(eval-when-compile
  (require 'use-package))

;; 安装 use-package 的集成模块
(use-package use-package-ensure-system-package
  :ensure t)
(use-package diminish
  :ensure t)
(use-package bind-key
  :ensure t)

;; 安装 benchmark-init, 优化Emacs启动速度
(require 'benchmark-init-modes)
(require 'benchmark-init)
(benchmark-init/activate)
;;; To disable collection of benchmark data after init is done.
(add-hook 'after-init-hook 'benchmark-init/deactivate)

;; 将lisp目录放在加载路径之前以提高启动速度
(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path (file-name-as-directory dir)))

;; 不要在 *message* 缓冲区显示加载模块化配置的信息
(with-temp-message ""
  (require 'init-ui)
  (require 'init-base)
  (require 'init-dired)
  (require 'init-edit)
  (require 'init-org)
  (require 'init-completion)
  (require 'init-tools)
  (require 'init-dev)
  (require 'init-research)
  (require 'init-eaf)
  (require 'init-ai)
  (require 'init-shell)
  )

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
   '(((:application eshell)
      eshell-connection-default-profile)))
 '(connection-local-profile-alist
   '((eshell-connection-default-profile
      (eshell-path-env-list))))
 '(package-selected-packages
   '(magit-delta which-key good-scroll org-contrib eshell-up doom-modeline pyvenv-auto shackle vertico org-appear diredfl marginalia org-roam-ui company-box lsp-ivy capf-autosuggest benchmark-init ivy-bibtex treemacs-projectile all-the-icons-dired counsel-projectile dashboard use-package-ensure-system-package org-roam-bibtex all-the-icons-completion evil eshell-syntax-highlighting minions helm-bibtex flycheck exec-path-from-shell eshell-git-prompt use-package-hydra cal-china-x restart-emacs pdf-tools rime keycast lsp-pyright yasnippet-snippets rainbow-delimiters org-noter diff-hl amx no-littering org-modern denote doom-themes auctex lsp-ui undo-tree crux fanyi org-auto-tangle org-present diminish dap-mode popper)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
