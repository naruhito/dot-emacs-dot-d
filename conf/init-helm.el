;;; helm.el --- Emacs incremental and narrowing framework -*- lexical-binding: t -*-
(when (<= 24 emacs-major-version) ;23以前には未対応
  (require 'utilities "~/.emacs.d/utilities")
  (add-to-load-path "~/.emacs.d/elisp/emacs-async")
  (add-to-load-path "~/.emacs.d/elisp/helm")

  ;; 基本設定
  (require 'helm-config)

  ;; bm を helm から利用する
  (require 'helm-bm)

  ;; Helm を有効にする
  (helm-mode 1)

  ;; helm-find-files 実行時に存在ファイルしないファイルを誤って指定してもバッファを作成しない
  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it))

  ;; helm-find-files 実行時に *scratch*, *eshell* 等、ファイルと関連付けられていないバッファを候補から除外 (必要な場合、switch-to-buffer)
  (setq helm-boring-buffer-regexp-list
        (list (rx (or (group "*" (+ not-newline) "*")
                      (group (+ not-newline) ".erb-template-indent-buffer"))))))
