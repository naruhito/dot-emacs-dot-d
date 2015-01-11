;;; web-mode.el --- major mode for editing html templates
(require 'web-mode)

;; 便利キー
; "C-c C-f" : HTMLタグを折り畳む/展開する
; "C-c C-n" : カーソルをHTMLタグの開始タグ/終了タグに移動させる

;; インデント設定
(defun web-mode-hook ()
  (setq web-mode-code-indent-offset 4))

(add-hook 'web-mode-hook  'web-mode-hook)
