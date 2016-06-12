;;; scala-mode2.el --- Major mode for editing scala
(when (<= 24 emacs-major-version) ;23以前には未対応
  (require 'utilities "~/.emacs.d/utilities")
  (add-to-load-path "~/.emacs.d/elisp/scala-mode2")
  (require 'scala-mode2)
  )

;;; sbt-mode.el --- Functions for discovering the current sbt project
(when (<= 24 emacs-major-version) ;23以前には未対応
  (require 'utilities "~/.emacs.d/utilities")
  (add-to-load-path "~/.emacs.d/elisp/sbt-mode")
  (require 'sbt-mode)
  )

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

(when (<= 24 emacs-major-version) ;23以前には未対応
  (require 'utilities "~/.emacs.d/utilities")
  (add-to-load-path "~/.emacs.d/elisp/ensime")
  (add-to-load-path "~/.emacs.d/elisp/company-mode")
  (require 'scala-mode2)
  (require 'ensime)
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

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
