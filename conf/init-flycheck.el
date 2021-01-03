;;; flycheck.el --- On-the-fly syntax checking

;; elisp 関連ファイルでは無効化
(defvar flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))
(require 'flycheck)
(require 'flycheck-popup-tip)

;; 全バッファで有効化
(add-hook 'after-init-hook #'global-flycheck-mode)

;; エラーが表示されるまでの時間
(setq flycheck-display-errors-delay 0.5)

;; エラーをポップアップで表示
(setq flycheck-display-errors-function
      (lambda (errors)
        (let ((messages (mapcar #'flycheck-error-message errors)))
          (popup-tip (mapconcat 'identity messages "\n")))))
