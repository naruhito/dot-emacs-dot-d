;;; Yasnippet.el --- Yet another snippet extension for Emacs.
(require 'yasnippet)

;; スニペットの保存場所
(setq yas-snippet-dirs
      '("~/.emacs.d/var/snippets"       ; 一時的な用途のスニペット
        "~/.emacs.d/snippets"))

;; 全バッファで有効化
(yas-global-mode 1)
