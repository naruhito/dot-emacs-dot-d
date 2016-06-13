;;; flycheck.el --- On-the-fly syntax checking
(when (<= 24 emacs-major-version) ;23以前には未対応
  (require 'utilities "~/.emacs.d/utilities")
  (add-to-load-path "~/.emacs.d/elisp/seq")
  (require 'flycheck)

  ;; 全バッファで有効化
  (add-hook 'after-init-hook #'global-flycheck-mode)

  ;; エラーが表示されるまでの時間
  (setq flycheck-display-errors-delay 0.5)

  ;; エラーをポップアップで表示
  (setq flycheck-display-errors-function
        (lambda (errors)
          (let ((messages (mapcar #'flycheck-error-message errors)))
            (popup-tip (mapconcat 'identity messages "\n"))))))
