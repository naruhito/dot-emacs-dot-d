;;; git-gutter.el --- Port of Sublime Text plugin GitGutter -*- lexical-binding: t; -*-

;; y/n で確認しない
(defvar git-gutter:ask-p nil)
(require 'git-gutter)

;; すべてのファイルで有効にする
(global-git-gutter-mode t)

;; diff ウィンドウの表示を切り換える関数
(defun git-gutter/custom:toggle-popup-hunk ()
  (interactive)
  (let ((gutter-buffer-name "*git-gutter:diff*"))
    (if (get-buffer gutter-buffer-name)
        (quit-windows-on gutter-buffer-name t)
      (git-gutter:popup-hunk))))
