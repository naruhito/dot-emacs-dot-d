;;; treesit-auto.el --- Automatically use tree-sitter enhanced major modes -*- lexical-binding: t -*-

(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install t)
  (global-treesit-auto-mode))
