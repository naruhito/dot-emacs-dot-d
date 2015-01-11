;;;; auto-async-byte-compile.el --- Automatically byte-compile when saved
(require 'auto-async-byte-compile)
(setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;; エラー・警告を表示する関数を登録 ("emacs-lisp-mode"においては、画面分割が行われていないことを想定)
(defun iaabc/display-function (buffer)
  (if (one-window-p)
      (split-window (selected-window) (round (* (window-height) 0.8))))
  (other-window 1)
  (switch-to-buffer buffer)
  (other-window 1))
(setq auto-async-byte-compile-display-function 'iaabc/display-function)

;; エラー・警告が0になった場合、通知用ウィンドウを閉じる
(defun iaabc/close-popup-window ()
  (with-current-buffer aabc/result-buffer
    (goto-char (point-min))
    (if (and (not (one-window-p)) (not (re-search-forward "\\.el[^c]" nil t)))
        (delete-window (get-buffer-window aabc/result-buffer)))))
(add-hook 'auto-async-byte-compile-hook 'iaabc/close-popup-window)
