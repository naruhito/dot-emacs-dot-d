;;; calendar.el --- calendar functions

;; ウィンドウだけでなく、バッファも削除する
(defadvice calendar-exit (after after-calendar-exit activate)
  (kill-buffer "*Calendar*"))
