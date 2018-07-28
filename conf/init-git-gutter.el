;;; git-gutter.el --- Port of Sublime Text plugin GitGutter -*- lexical-binding: t; -*-

;; y/n で確認しない
(defvar git-gutter:ask-p nil)
(require 'git-gutter)

;; すべてのファイルで有効にする
(global-git-gutter-mode t)
