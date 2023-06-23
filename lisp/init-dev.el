;;; init-dev.el --- Development settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package projectile
  :ensure t
  :bind (("C-c p" . projectile-command-map))
  :config
  (setq projectile-mode-line "Projectile")
  (setq projectile-track-known-projects-automatically nil))

(use-package counsel-projectile
  :ensure t
  :after (projectile)
  :init (counsel-projectile-mode))

;; all-the-icons
;; @source https://github.com/domtronn/all-the-icons.el#installation
;; On Windows, after downloading the fonts, one needs to manually install them
;; M-x package-install RET all-the-icons
;; restart-emacs, M-x all-the-icons-install-fonts
(use-package all-the-icons
  :if (display-graphic-p))

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

;; magit
;; DEPENDENCY @ https://github.com/dandavison/delta
;; INSTALL delta @ https://dandavison.github.io/delta/installation.html
;; EDIT ~/.gitconfig @ https://dandavison.github.io/delta/get-started.html
(use-package magit
  :ensure t
  :hook (git-commit-mode . flyspell-mode)
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch))
  :custom
  (magit-diff-refine-hunk t)
  (magit-ediff-dwim-show-on-hunks t))

;; diff-hl
(use-package diff-hl
  :ensure t
  :hook ((dired-mode         . diff-hl-dired-mode-unless-remote)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (global-diff-hl-mode t)
  :config
  ;; When Emacs runs in terminal, show the indicators in margin instead.
  (unless (display-graphic-p)
    (diff-hl-margin-mode)))

(use-package magit-delta
  :ensure t
  :hook (magit-mode . magit-delta-mode)
  :config
  (setq magit-delta-hide-plus-minus-markers nil)
  )

(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package elisp-mode
  :ensure nil
  :after org
  :bind (:map emacs-lisp-mode-map
              ("C-c C-b" . eval-buffer)
              ("C-c C-c" . eval-to-comment)
              :map lisp-interaction-mode-map
              ("C-c C-c" . eval-to-comment)
              :map org-mode-map
              ("C-c C-;" . eval-to-comment)
              )
  :init
  ;; for emacs-lisp org babel
  (add-to-list 'org-babel-default-header-args:emacs-lisp
             '(:results . "value pp"))
  :config
  (defconst eval-as-comment-prefix " ⇒ ")
  (defun eval-to-comment (&optional arg)
    (interactive "P")
    ;; (if (not (looking-back ";\\s*"))
    ;;     (call-interactively 'comment-dwim))
    (call-interactively 'comment-dwim)
    (progn
      (search-backward ";")
      (forward-char 1))
    (delete-region (point) (line-end-position))
    (save-excursion
      (let ((current-prefix-arg '(4)))
        (call-interactively 'eval-last-sexp)))
    (insert eval-as-comment-prefix)
    (end-of-line 1))
  )

(add-to-list 'load-path "~/.emacs.d/site-lisp/smart-align/")
(require 'smart-align)

(add-to-list 'load-path "~/.emacs.d/site-lisp/effortless-indent/")
(require 'effortless-indent)

(add-to-list 'load-path "~/.emacs.d/site-lisp/awesome-pair/")
(require 'awesome-pair)

(dolist (hook (list
               'c-mode-common-hook
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'haskell-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'maxima-mode-hook
               'ielm-mode-hook
               'sh-mode-hook
               'makefile-gmake-mode-hook
               'php-mode-hook
               'python-mode-hook
               'js-mode-hook
               'go-mode-hook
               'qml-mode-hook
               'jade-mode-hook
               'css-mode-hook
               'ruby-mode-hook
               'coffee-mode-hook
               'rust-mode-hook
               'qmake-mode-hook
               'lua-mode-hook
               'swift-mode-hook
               'minibuffer-inactive-mode-hook
               ))
  (add-hook hook '(lambda () (awesome-pair-mode 1))))

(define-key awesome-pair-mode-map (kbd "(") 'awesome-pair-open-round)
(define-key awesome-pair-mode-map (kbd "[") 'awesome-pair-open-bracket)
(define-key awesome-pair-mode-map (kbd "{") 'awesome-pair-open-curly)
(define-key awesome-pair-mode-map (kbd ")") 'awesome-pair-close-round)
(define-key awesome-pair-mode-map (kbd "]") 'awesome-pair-close-bracket)
(define-key awesome-pair-mode-map (kbd "}") 'awesome-pair-close-curly)
(define-key awesome-pair-mode-map (kbd "%") 'awesome-pair-match-paren)
(define-key awesome-pair-mode-map (kbd "\"") 'awesome-pair-double-quote)
(define-key awesome-pair-mode-map (kbd "M-o") 'awesome-pair-backward-delete)
(define-key awesome-pair-mode-map (kbd "C-k") 'awesome-pair-kill)
(define-key awesome-pair-mode-map (kbd "M-\"") 'awesome-pair-wrap-double-quote)
(define-key awesome-pair-mode-map (kbd "M-[") 'awesome-pair-wrap-bracket)
(define-key awesome-pair-mode-map (kbd "M-{") 'awesome-pair-wrap-curly)
(define-key awesome-pair-mode-map (kbd "M-(") 'awesome-pair-wrap-round)
(define-key awesome-pair-mode-map (kbd "M-)") 'awesome-pair-unwrap)
(define-key awesome-pair-mode-map (kbd "M-p") 'awesome-pair-jump-right)
(define-key awesome-pair-mode-map (kbd "M-n") 'awesome-pair-jump-left)
(define-key awesome-pair-mode-map (kbd "M-:") 'awesome-pair-jump-out-pair-and-newline)

(add-to-list 'load-path "~/.emacs.d/site-lisp/color-rg/")
(require 'color-rg)

(unless (package-installed-p 'evil)
  (package-install 'evil))
(require 'evil)
(evil-mode 1)

;; 适配 org-mode
(setq org-adapt-indentation t)
(define-key evil-normal-state-map (kbd "<tab>") 'org-cycle)

;; 适配 treemacs / dashboard
(evil-set-initial-state 'treemacs-mode 'emacs)
(evil-set-initial-state 'dashboard-mode 'emacs)

(provide 'init-dev)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dev.el ends here
