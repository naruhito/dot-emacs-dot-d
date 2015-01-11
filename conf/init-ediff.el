;;; ediff.el --- a comprehensive visual interface to diff & patch
(require 'ediff)

;; コントロールパネル専用のフレームを作成しない
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; 起動時に分割方法を変更する
(add-hook 'ediff-startup-hook 'ediff-toggle-split)
