;;; scala-mode.el --- Major mode for editing Scala
(when (<= 24 emacs-major-version) ;23以前には未対応
  (require 'utilities "~/.emacs.d/utilities")
  (add-to-load-path "~/.emacs.d/elisp/scala-mode")
  (require 'scala-mode)
  )

;;; sbt-mode.el --- Interactive support for sbt projects
(when (<= 24 emacs-major-version) ;23以前には未対応
  (require 'utilities "~/.emacs.d/utilities")
  (add-to-load-path "~/.emacs.d/elisp/sbt-mode")
  (require 'sbt-mode)
  )

;;; ensime.el --- ENhanced Scala Interaction Mode for Emacs

;;; 基本コマンド

;; 情報の調査

;; - `C-c C-v e ` カーソル上の警告やエラーの内容を表示
;; - `C-c C-v t (or T)` カーソル上の要素の型を表示
;; - `C-c C-v d` カーソル上の要素のドキュメントをブラウザ表示

;; インポート

;; - `C-c C-r t` カーソル上の要素用の import 候補を表示
;; - `C-c C-v v` パッケージを検索して import を挿入
;; - `C-c C-r o` import 設定を整理

;; リファクタリング

;; - `C-c C-r r` 変数名を一括編集

;; その他

;; - `C-c C-b c` エラーチェック (コンパイル)
;; - `M-.` 定義にジャンプ
;; - `M-,` ジャンプもとに戻る
;; - `C-c C-v .` 範囲選択 ("." と "," で拡大縮小)

(when (<= 24 emacs-major-version) ;23以前には未対応
  (require 'utilities "~/.emacs.d/utilities")
  (add-to-load-path "~/.emacs.d/elisp/ensime")
  (add-to-load-path "~/.emacs.d/elisp/company-mode")
  (require 'scala-mode)
  (require 'ensime)
  (add-hook 'scala-mode-hook 'ensime-mode)
  (setq ensime-startup-snapshot-notification nil)
  (setq ensime-startup-notification nil)

  ;; ドット '.' を補完機能のトリガーに設定
  (defun scala/completing-dot-company ()
    (eval-and-compile (require 'company))
    (cond (company-backend
           (company-complete-selection)
           (scala/completing-dot))
          (t
           (insert ".")
           (company-complete))))
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
          ((eq ensime-completion-style 'company)
           (scala/completing-dot-company))))
  (define-key scala-mode-map (kbd ".") 'scala/completing-dot))
