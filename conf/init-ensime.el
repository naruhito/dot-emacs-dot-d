;;; ensime.el --- ENhanced Scala Interaction Mode for Emacs
;; ensime-server を http://ensime.typelevel.org/ から取得して ~/.emacs.d/ensime/ に保存
;; 参考:  http://ensime.github.io/editors/emacs/install/#installing-the-server-from-assembly-builds-or-source
;;        > SNAPSHOT assembly jars are provided at http://ensime.typelevel.org/

;;; 基本コマンド

;; 情報の調査

;; - `C-c C-v i` カーソル上の要素の情報を表示
;; - `C-c C-v t` カーソル上の要素の型を表示
;; - `C-c C-v d` カーソル上の要素のドキュメントをブラウザ表示

;; インポート

;; - `C-c C-r t` カーソル上の要素用の import 候補を表示
;; - `C-c C-v v` パッケージを検索して import を挿入
;; - `C-c C-r o` import 設定を整理

;; リファクタリング

;; - `C-c C-r r` 変数名を一括編集

;; その他

;; - `C-c C-c a` エラーチェック
;; - `M-.` 定義にジャンプ
;; - `M-,` ジャンプもとに戻る
;; - `C-c C-v .` 範囲選択 ("." と "," で拡大縮小)

(require 'utilities "~/.emacs.d/utilities")
(add-to-load-path "~/.emacs.d/elisp/ensime")

(when (<= 24 emacs-major-version) ;23以前には未対応
  (require 'scala-mode2)
  (require 'auto-complete)
  (require 'ensime)
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

  ;; ensime の補完に auto-complete を使用する
  (setq ensime-completion-style 'auto-complete)

  ;; ドット '.' を補完機能のトリガーに設定
  (defun scala/completing-dot ()
    "Insert a period and show company completions."
    (interactive "*")
    (eval-and-compile (require 'ensime))
    (eval-and-compile (require 's))
    (when (s-matches? (rx (+ (not space)))
                      (buffer-substring (line-beginning-position) (point)))
      (delete-horizontal-space t))
    (cond ((not (and (ensime-connected-p) ensime-completion-style))
           (insert "."))
          ((eq ensime-completion-style 'auto-complete)
           (insert ".")
           (ac-trigger-key-command t))))
  (define-key scala-mode-map (kbd ".") 'scala/completing-dot))
