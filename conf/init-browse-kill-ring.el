;;; browse-kill-ring.el --- interactively insert items from kill-ring
(require 'browse-kill-ring)

;; browse-kill-ring 終了時にバッファを kill する
(setq browse-kill-ring-quit-action 'kill-and-delete-window)
