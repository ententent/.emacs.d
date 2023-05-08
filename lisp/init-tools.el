;;; init-tools.el --- Tools settings -*- lexical-binding: t -*-
;;; Commentary: Useful tools to make Emacs efficient!

;;; Code:

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package restart-emacs
  :ensure t)

(use-package which-key
  :ensure t
  :init (which-key-mode))

;; dependency
;; sudo apt install fcitx5 fcitx5-* librime-dev
;; cd ~/.local/share/fcitx5/rime
;; mv rime rime_bak/
;; git clone https://github.com/iDvel/rime-ice --depth=1
;; mv rime-ice rime/
(add-to-list 'load-path "~/.emacs.d/emacs-rime/")
(require 'rime)

;; 雾凇拼音
;;; https://emacs-china.org/t/emacs-rime/24125
;;; https://github.com/iDvel/rime-ice

;;; Code:
(setq rime-user-data-dir "~/.local/share/fcitx5/rime")

(setq rime-posframe-properties
      (list :background-color "#333333"
            :foreground-color "#dcdccc"
            ;;:font "WenQuanYi Micro Hei Mono-14"
            :internal-border-width 10))

(setq default-input-method "rime"
      rime-show-candidate 'posframe)

(use-package fanyi
  :ensure t
  :bind-keymap ("\e\e =" . fanyi-map)
  :bind (:map fanyi-map
              ("w" . fanyi-dwim2)
              ("i" . fanyi-dwim))
  :init
  ;; to support `org-store-link' and `org-insert-link'
  (require 'ol-fanyi)
  ;; 如果当前指针下有单词，选择当前单词，否则选择剪贴板
  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 '("w" "New word" entry (file+olp+datetree "20221001T221032--vocabulary__studying.org" "New")
                   "* %^{Input the new word:|%(cond ((with-current-buffer (org-capture-get :original-buffer) (thing-at-point 'word 'no-properties))) ((clipboard/get)))}\n\n[[fanyi:%\\1][%\\1]]\n\n[[http://dict.cn/%\\1][海词：%\\1]]%?"
                   :tree-type day
                   :empty-lines 1
                   :jump-to-captured t)))
  :config
  (defvar fanyi-map nil "keymap for `fanyi")
  (setq fanyi-map (make-sparse-keymap))
  (setq fanyi-sound-player "mpv")
  :custom
  (fanyi-providers '(;; 海词
                     fanyi-haici-provider
                     ;; 有道同义词词典
                     fanyi-youdao-thesaurus-provider
                     ;; Longman
                     fanyi-longman-provider
                     ;; ;; Etymonline
                     ;; fanyi-etymon-provider
                     ;; ;; LibreTranslate
                     ;; fanyi-libre-provider
                     )))

(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'beancount)
(add-to-list 'auto-mode-alist '("\\.bean\\'" . beancount-mode))
;; (add-hook 'beancount-mode-hook
;;   (lambda () (setq-local electric-indent-chars nil)))
(add-hook 'beancount-mode-hook #'outline-minor-mode)
(define-key beancount-mode-map (kbd "C-c C-n") #'outline-next-visible-heading)
(define-key beancount-mode-map (kbd "C-c C-p") #'outline-previous-visible-heading)

(use-package evil
  :ensure t
  :defer 1
  :preface
  (setq evil-want-visual-char-semi-exclusive t
        evil-echo-state t
        evil-symbol-word-search t
        evil-want-C-i-jump nil
        org-adapt-indentation t)
  :config
  (evil-mode 1))

(provide 'init-tools)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tools.el ends here
