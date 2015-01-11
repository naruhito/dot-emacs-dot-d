;;; auto-save-buffers.el -- 自動保存機能追加
(require 'auto-save-buffers)

;; アイドル0.5秒で保存
(run-with-idle-timer 0.5 t 'auto-save-buffers)
