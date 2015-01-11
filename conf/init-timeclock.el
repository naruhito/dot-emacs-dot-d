;;; timeclock.el --- mode for keeping track of how much you work
(require 'timeclock)
(require 'time)

;; 時刻の表示
(setq display-time-string-forms 
      '((format "%s/%s/%s(%s) %s:%s" 
                year month day dayname 24-hours minutes)))
(display-time) 

;; Timelogの保存場所
(setq timeclock-file "~/.emacs.d/var/timelog")

;; 現在の状態を保存する変数
(defvar timeclock-current-status)

;; モードラインに現在の状態を表示する
(setq global-mode-string
      (append global-mode-string '(timeclock-current-status)))

;; 現在の状態を更新する関数
(defun timeclock-current-status-update-modeline ()
  (interactive)
  (let ((last-in (equal (nth 0 timeclock-last-event) "i"))
        (project (nth 2 timeclock-last-event))
        (since (format-time-string "%H:%M"
                                   (nth 1 timeclock-last-event))))
    (setq timeclock-current-status
          (if (not last-in) ""
            (format "--(In/%s since %s)" project since)))
    (force-mode-line-update)))
(add-hook 'timeclock-in-hook 'timeclock-current-status-update-modeline)
(add-hook 'timeclock-out-hook 'timeclock-current-status-update-modeline)
