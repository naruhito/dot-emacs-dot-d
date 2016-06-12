;;; Yasnippet.el --- Yet another snippet extension for Emacs.
(require 'yasnippet)
(yas-global-mode 1)

;; スニペットの保存場所
(setq yas-snippet-dirs
      '("~/.emacs.d/var/snippets"       ; 一時的な用途のスニペット
        "~/.emacs.d/snippets"))
