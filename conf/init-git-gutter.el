;;; git-gutter.el --- Port of Sublime Text plugin GitGutter -*- lexical-binding: t; -*-
(when (<= 24 emacs-major-version) ;23以前には未対応
  (require 'git-gutter)
  ;; すべてのファイルで有効にする
  (global-git-gutter-mode t))
