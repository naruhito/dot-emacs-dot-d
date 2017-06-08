;;; real-auto-save.el --- Automatically save your all your buffers/files at regular intervals.
(require 'real-auto-save)
(add-hook 'find-file-hook 'real-auto-save-mode)

;; アイドル 0.5 秒で保存
(setq real-auto-save-interval 0.5)
