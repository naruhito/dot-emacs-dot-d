;;; less-css-mode.el --- Major mode for editing LESS CSS files (lesscss.org)
(require 'less-css-mode)

;; C-c C-cでのコンパイルオプション (1行に圧縮)
(setq less-css-lessc-options (list "--yui-compress"))

;; ウィンドウズでのコンパイルエラーに対応
(if (eq system-type 'windows-nt)
    (defun less-css-compile ()
      "Compiles the current buffer to css using `less-css-lessc-command'."
      (interactive)
      (message "Compiling less to css")
      (compile
       (mapconcat 'identity
                  (append (list less-css-lessc-command) ;(修正) quoteをつけない
                          less-css-lessc-options
                          (list (shell-quote-argument buffer-file-name)
                                ">"
                                (shell-quote-argument (less-css--output-path))))
                  " "))))
