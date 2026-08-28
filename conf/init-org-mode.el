;;; org.el --- Outline-based notes management and organizer -*- lexical-binding: t; -*-

;; アジェンダで読み込むOrgファイルのパスを指定
;; ~/.emacs.d/var/notes/ 以下のすべての .org ファイルを再帰的に取得する
(setq org-agenda-files
      (directory-files-recursively "~/.emacs.d/var/notes/" "\\.org$"))

;; org-downloadの設定
(require 'org-download)

;; diredモードでファイルをドラッグ＆ドロップできるようにする
(add-hook 'dired-mode-hook 'org-download-enable)

;; org-modeで画像をドラッグ＆ドロップできるようにする
(add-hook 'org-mode-hook 'org-download-enable)
(setq-default org-download-image-dir "~/.emacs.d/var/notes/images")

;; org-modeで画像をインライン表示する
(setq org-startup-with-inline-images t)

;; 画像の幅を window にする
(setq org-image-max-width 'window)

;; クリップボードからの取得ツールを指定
;; windowsで IrfanView の i_view64.exe を使う場合 https://www.irfanview.com/
(if (eq system-type 'windows-nt)
    (setq org-download-screenshot-method "C:/Program Files/IrfanView/i_view64.exe /clippaste /convert=\"%s\""))
