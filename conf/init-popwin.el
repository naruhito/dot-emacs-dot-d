;;; popwin.el --- Popup Window Manager.
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

;; C-gでウィンドウを閉じた後、それが以下で登録されているバッファであれば削除まで行う
(defadvice popwin:close-popup-window (after after-popwin:close-popup-window activate)
  (mapc '(lambda (buffer)
           (if (get-buffer buffer)
               (kill-buffer buffer)))
        (list "*compilation*" "*Completions*" "*Help*")))
