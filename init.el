;;; init.el --- The main init entry for Emacs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(require 'package)
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

;; 将lisp目录放在加载路径之前以提高启动速度
(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path (file-name-as-directory dir)))

;; 不要在 *message* 缓冲区显示加载模块化配置的信息
(with-temp-message ""
  (require 'init-ui)
  (require 'init-base)
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
 '(package-selected-packages
   '(eshell-up capf-autosuggest eshell-syntax-highlighting eshell-git-prompt yasnippet which-key use-package-ensure-system-package undo-tree treemacs-projectile restart-emacs rainbow-delimiters pdf-tools org-preview-html org-noter org-modern org-contrib org-auto-tangle org-appear no-littering mwim minions marginalia magit-delta lsp-treemacs keycast grip-mode fanyi exec-path-from-shell evil-org evil-collection elfeed-goodies doom-themes doom-modeline diminish diff-hl denote dashboard crux counsel-projectile cnfonts cal-china-x auctex all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
