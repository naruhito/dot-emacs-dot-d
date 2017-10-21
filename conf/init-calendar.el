;;; calendar.el --- calendar functions

;; ウィンドウだけでなく、バッファも削除する
(defadvice calendar-exit (after after-calendar-exit activate)
  (kill-buffer "*Calendar*"))

;; 不要なヘルプ文字列を非表示にする
(defvar calendar-date-echo-text nil)
