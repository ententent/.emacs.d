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

;; (use-package evil
;;   :ensure t
;;   :defer 1
;;   :preface
;;   (setq evil-want-visual-char-semi-exclusive t
;;         evil-echo-state t
;;         evil-symbol-word-search t
;;         evil-want-C-i-jump nil
;;         org-adapt-indentation t)
;;   :config
;;   (evil-mode 1))

(provide 'init-tools)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tools.el ends here
