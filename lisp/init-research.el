;;; init-research.el --- Research settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(setq zot_bib '("~/research/LFH/TeX/ref.bib"
                ; Zotero 用 Better BibTeX 导出的 .bib 文件. 可以是多个文件
                "~/research/MAGE/TeX/ref.bib")
      ; Zotero 的 ZotFile 同步文件夹
      zot_pdf "~/MEGA/Zotero-Library"
      ; 自定义的 org-roam 文献笔记目录
      org_refs "~/org/roam/ref/" )

(use-package ivy-bibtex ; 这里也可以用 ivy-bibtex 替换 helm-bibtex
  :ensure t
  :custom
  (bibtex-completion-notes-path org_refs)
  (bibtex-completion-bibliography zot_bib)
  (bibtex-completion-library-path zot_pdf))

(use-package org-roam-bibtex
  :ensure t
  :hook (org-roam-mode . org-roam-bibtex-mode)i
  :bind (("C-c n k" . orb-insert-link)
         ("C-c n a" . orb-note-actions))
  :custom
  ; 与上面 helm-bibtex/ivy-bibtex 的选择保持一致
  (orb-insert-interface 'ivy-bibtex)
  ; 默认使用标题, 但是论文的标题一般很长, 不适合作为笔记链接的名字
  (orb-insert-link-description 'citekey)
  (orb-preformat-keywords
   '("citekey" "title" "url" "author-or-editor" "keywords" "file"))
  (orb-process-file-keyword t)
  (orb-attached-file-extensions '("pdf")))

;; 卡片笔记法的 org-roam 实践
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/org/roam/")               ;; 默认笔记目录
  (org-roam-dailies-directory "daily/")            ;; 默认日记目录，默认笔记目录的相对路径
  (org-roam-db-gc-threshold most-positive-fixnum)  ;; 提升性能
  :bind (("C-c n f" . org-roam-node-find)          ;; 通过关键字查找笔记并跳转
         ("C-c n i" . org-roam-node-insert)        ;; 插入一条笔记的链接或创建一条笔记
         ("C-c n c" . org-roam-capture)            ;; 根据预设模板创建 org 格式的笔记
         ("C-c n l" . org-roam-buffer-toggle)      ;; 显示后链窗口
         ("C-c n u" . org-roam-ui-mode))           ;; 浏览器中可视化
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)               ;; 日记菜单
  :config
  (require 'org-roam-dailies)                      ;; 启用日记功能
  (org-roam-db-autosync-mode))                     ;; 启动时自动同步数据库

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)                       ;; 同步 Emacs 主题
  (org-roam-ui-follow t)                           ;; 笔记节点跟随
  (org-roam-ui-update-on-save t))

;; org-roam 笔记模板
(setq org-roam-capture-templates
             '(("d" "default" plain "- tag :: \n %?" ; 普及模板
                :target
                (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title} \n")
                :unnarrowed t)
               ("r" "bibliography reference in pdfs" plain ; 文献模板
                "#+FILETAGS: reading research \n - tags :: %^{keywords} \n* %^{title}\n:PROPERTIES:\n:Custom_ID: %^{citekey}\n:URL: %^{url}\n:AUTHOR: %^{author-or-editor}\n:NOTER_DOCUMENT: ~/MEGA/Zotero-Library/%^{citekey}.pdf\n:NOTER_PAGE:\n:END:"      
                :target
                (file+head "ref/${citekey}.org" "#+title: ${title}\n"))))

;; 对于Windows系统，需要安装ImageMagick，并保证magick.exe在PATH变量的路径中
;;; 用msys2安装: pacman -S mingw-w64-x86_64-imagemagick
(use-package org-download
  :ensure async ;; 因为不是从melpa安装，需要手动保证async安装
  :defer t ;; 延迟加载
  :load-path "~/.emacs.d/lisp/"
  :bind
  (:map org-mode-map
        ("C-M-y" . org-download-clipboard)) ;; 绑定从剪贴板粘贴截图的快捷键
  :custom
  (org-download-heading-lvl 1) ;; 用一级标题给截图文件命名
  :config
  ;; 用同级 ./img 目录放置截图文件
  (setq-default org-download-image-dir "./img"))

  ;; 打开 TeX 文件时应执行的命令
  (defun my-latex-hook ()
    (turn-on-cdlatex)          ;; 加载cdlatex
    (turn-on-reftex)           ;; 加载reftex
    (prettify-symbols-mode t)  ;; 加载prettify-symbols-mode
    (outline-minor-mode)       ;; 加载outline-mode
    (outline-hide-body))       ;; 打开文件时只显示章节标题

  ;; tex
  ;; @source https://github.com/jwiegley/use-package/issues/379#issuecomment-246161500
  (use-package tex
    :defer t
    :ensure auctex
    :custom
    ;; 对新文件自动解析 (usepackage, bibliograph, newtheorem) 等信息
    (TeX-parse-selt t)
    (TeX-PDF-mode t)
    ;; 正向与反向搜索设置
    (TeX-source-correlate-mode t)
    (TeX-source-correlate-method 'synctex)
    ;; 使用 eaf-pdf-viewer 打开PDF
    (TeX-view-program-selection '((output-pdf "eaf")))
    :config
    (setq TeX-auto-save t)
    ;; 编译时问询主文件名称
    (setq-default TeX-master t)                               
    ;; 编译后更新pdf文件
    (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
    ;; 加载LaTeX模式设置
    (add-hook 'LaTeX-mode-hook 'my-latex-hook)
    ;; XeLaTeX 支持
    (add-hook 'LaTeX-mode-hook (lambda()
                                 (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
                                 (add-to-list 'TeX-view-program-list '("eaf" eaf-pdf-synctex-forward-view))
                                 (setq TeX-save-query nil
                                       TeX-show-compilation t))))

(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'cdlatex)

;; customize prettify
;; https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode
;; 可加入原列表中没有的编码、简化常用命令
(require 'tex-mode) ;; 载入 tex--prettify-symbols-alist 变量
(defun my/more-prettified-symbols ()
  (mapc (lambda (pair) (delete pair tex--prettify-symbols-alist))
        '(("\\supset" . 8835)))
  (mapc (lambda (pair) (cl-pushnew pair tex--prettify-symbols-alist))
        '(("\\Z" . 8484)
          ("\\Q" . 8474)
          ("\\N" . 8469)
          ("\\R" . 8477)
          ("\\eps" . 949)
          ("\\inf" . #x22C0) 
          ("\\sup". #x22C1)
          ("\\ONE" . #x1D7D9)
          ("\\mathbb{S}" . #x1D54A)
          ("\\PP" . #x2119)
          ("\\Ps" . #x1D5AF )
          ("\\Pp" . #x1D40F)
          ("\\E" . #x1D5A4)
          ("\\Ee" . #x1D404)
          ("\\EE" . #x1D53C )
          ("\\Fc" . #x2131)
          ("\\Nc" . #x1D4A9))))
(my/more-prettified-symbols) ;; 读入自定义 prettify 符号

(setq prettify-symbols-unprettify-at-point t)  ;; 自动展开光标附近的宏命令

(use-package org-present
  :defer t
  :config
  (defun my/org-present-prepare-slide (buffer-name heading)
    (org-overview)  ;; 仅显示顶层标题Show only top-level headlines
    (org-show-entry);; 展开当前标题Unfold the current entry
    (org-show-children))   ;; 显示当前子标题

  (defun my/org-present-start () ;; 开始幻灯片的设置
    (setq visual-fill-column-width 110
      visual-fill-column-center-text t) ;; 调整显示界面
    ;; 调整字体大小
    (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                       (header-line (:height 4.0) variable-pitch)
                                       (org-document-title (:height 1.75) org-document-title)
                                       (org-code (:height 1.55) org-code)
                                       (org-verbatim (:height 1.55) org-verbatim)
                                       (org-block (:height 1.25) org-block)
                                       (org-block-begin-line (:height 0.7) org-block)))
    (setq header-line-format " ") ;; 在标题前加入空行
    (org-display-inline-images) ;; 显示图片
    (flyspell-mode 0) ;; 禁用拼写检查 (防止标红影响效果)
    (read-only-mode 1) ;; 只读模式
    )

  (defun my/org-present-end () ;; 重置上述设置
    (setq-local face-remapping-alist 
                '((default variable-pitch default)))      
    (setq header-line-format nil) 
    (org-remove-inline-images)
    (org-present-small)
    (flyspell-mode t)
    (read-only-mode 0))
  
  (add-hook 'org-present-mode-hook 'my/org-present-start)
  (add-hook 'org-present-mode-quit-hook 'my/org-present-end)
  (add-hook 'org-present-after-navigate-functions 'my/org-present-prepare-slide))

(provide 'init-research)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-research.el ends here
