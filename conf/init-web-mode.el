;;; web-mode.el --- major mode for editing html templates
(require 'web-mode)

;; 便利キー
; "C-c C-f" : HTMLタグを折り畳む/展開する
; "C-c C-n" : カーソルをHTMLタグの開始タグ/終了タグに移動させる

;; インデント設定
(defun web-mode-hook ()
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset nil)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-sql-indent-offset 2)
  (setq indent-tabs-mode nil)
  (setq tab-width 2))

(add-hook 'web-mode-hook  'web-mode-hook)

;; コメント設定
(add-to-list 'web-mode-comment-formats '("jsx" . "//" ))

;; '=' の後に "" を自動追加しない。
(setq web-mode-enable-auto-quoting nil)
