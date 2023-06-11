  ;;; early-init.el --- Emacs pre-initialization config -*- lexical-binding: t -*-
  ;;; Commentary:

  ;;; Code:

  ;; 设置垃圾回收参数
  (setq gc-cons-threshold most-positive-fixnum)
  ;;; (setq gc-cons-percentage 0.6)
  ;; 清空避免加载远程文件的时候分析文件
  (setq file-name-handler-alist nil)

  ;; 启动早期不加载`package.el'包管理器
  (setq package-enable-at-startup nil)
  ;; 不从包缓存中加载
  (setq package-quickstart nil)

  ;; 禁止展示菜单栏、工具栏和纵向滚动条
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)

  ;; 禁止自动缩放窗口先
  (setq frame-inhibit-implied-resize t)

  ;; 禁止菜单栏、工具栏、滚动条模式，禁止启动屏幕和文件对话框
  (menu-bar-mode -1)                      ; 关闭菜单栏
  (tool-bar-mode -1)                      ; 关闭工具栏
  (scroll-bar-mode -1)                    ; 关闭滚动条
  (setq inhibit-splash-screen t)
  (setq use-file-dialog nil)

  ;; 在这个阶段不编译
  (setq comp-deferred-compilation nil)

  ;; 启动时窗口全屏
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  (provide 'early-init)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; early-init.el ends here
