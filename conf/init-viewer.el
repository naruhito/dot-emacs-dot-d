;;; viewer.el --- View-mode extension
(require 'viewer)

;; C-x C-r (find-file-read-only) で開くときはview-modeを有効にする
(setq view-read-only t)

;; 書き込み不能なファイルではview-modeから抜けられないようにする
(viewer-stay-in-setup)

;; モードラインに色を付ける
(setq viewer-modeline-color-unwritable "orange"
      viewer-modeline-color-view "orange")
(viewer-change-modeline-color-setup)

;; 指定した拡張子は、view-modeで開く
(setq view-mode-by-default-regexp "\\(\\.el\\.gz\\|\\.log\\)$")

;; view.elでコメントアウトされていた関数。警告が表示されるからという理由で除外された？
(require 'view)
(defun View-goto-line-last (&optional line)
"Move to last (or prefix LINE) line in View mode.
Display is centered at LINE.
Sets mark at starting position and pushes mark ring."
 (interactive "P")
 (push-mark)
 (if line (goto-line (prefix-numeric-value line))
   (goto-char (point-max))
   (beginning-of-line))
 (view-recenter))
