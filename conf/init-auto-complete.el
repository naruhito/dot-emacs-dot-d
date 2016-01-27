;;; auto-complete.el --- Auto Completion for GNU Emacs
(require 'auto-complete-config)

;; 各種候補を表示するまでの遅延時間
(setq ac-delay 0.0)
(setq ac-auto-show-menu 0.0)
(setq ac-quick-help-delay 0.0)

;; ファイルを開くと同時にACモードを有効にするなど
(add-hook 'find-file-hooks 'iac/ac-setup)
(add-hook 'after-revert-hook 'iac/ac-setup)

;; ACモードの初期設定
(defun iac/ac-setup ()
  (auto-complete-mode)
  (setq ac-sources
        (append
         (list
          ac-source-words-in-all-buffer
          ac-source-symbols
          )
         ac-sources)))

;; ヒストリの保存場所を変更
(setq ac-comphist-file "~/.emacs.d/var/ac-comphist.dat")
