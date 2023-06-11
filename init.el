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

;; 抹掉插件启动的输出
(with-temp-message ""
  (require 'init-ui)
  (require 'init-ai)
  (require 'init-base)
  (require 'init-dev)
  (require 'init-eaf)
  (require 'init-edit)
  (require 'init-english)
  (require 'init-finance)
  (require 'init-lsp)
  (require 'init-org)
  (require 'init-python)
  (require 'init-research)
  (require 'init-tools)

  ;; Restore session at last.
  (require 'init-session)
  (emacs-session-restore)
  )

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yasnippet-snippets use-package-hydra use-package-ensure-system-package treemacs-projectile treemacs-all-the-icons tablist shackle request rainbow-delimiters queue pyvenv popper polymode password-store-otp ox-hugo org-roam-ui org-roam-bibtex org-modern org-drill org-contrib org-appear org-ai no-littering minions marginalia magit-delta lsp-ui lsp-pyright lsp-ivy ivy-bibtex graphql good-scroll eshell-up eshell-syntax-highlighting eshell-git-prompt diminish diff-hl dashboard crux counsel-projectile conda company-box capf-autosuggest cal-china-x benchmark-init auctex async anaphora amx all-the-icons-gnus all-the-icons-completion alert aio)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
