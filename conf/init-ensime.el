;; Scala 開発環境 Ensime の設定 (v24 のみ対応; 設定は任意です)
;; ==================
;; 一般的な sbt の設定を行った後に、追加で以下の設定を行います。

;; ### ensime-sbt グローバルプラグインの設定

;; 	$ vi ~/.sbt/0.13/plugins/plugins.sbt
;; 	addSbtPlugin("org.ensime" % "sbt-ensime" % "1.12.14")

;; 	$ vi ~/.sbt/0.13/global.sbt
;; 	import org.ensime.EnsimeKeys._
;; 	ensimeIgnoreMissingDirectories := true

;; ### 使用方法
;; Scala プロジェクトに移動した後、初回のみ `.ensime` ファイルを生成してください。

;; 	$ cd /path/to/your-scala-project
;; 	$ sbt ensimeConfig

;; `ensime-server` を起動してください。

;; 	M-x ensime

;; 終了したい場合は以下のようにします。

;; 	M-x ensime-shutdown

;; build.sbt の `libraryDependencies` を変更した場合 `.ensime` ファイルを更新して ensime 設定を再読み込みしてください。

;; 	$ cd /path/to/your-scala-project
;; 	$ sbt ensimeConfig
;; 	M-x ensime-reload

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
  (define-key scala-mode-map (kbd ".") 'scala/completing-dot)

  ;; `ctags` を利用した定義ジャンプ http://blog.shibayu36.org/entry/2017/08/21/080047
  ;; => `C-@` helm-etags-plus-select
  ;;
  ;; インストール
  ;; $ brew install ctags
  ;; $ touch ~/.ctags
  ;; --langdef=scala
  ;; --langmap=scala:.scala
  ;; --regex-scala=/^[ \t]*((abstract|final|sealed|implicit|lazy)[ \t]*)*(private[^ ]*|protected)?[ \t]*class[ \t]+([a-zA-Z0-9_]+)/\4/c,classes/
  ;; --regex-scala=/^[ \t]*((abstract|final|sealed|implicit|lazy)[ \t]*)*(private[^ ]*|protected)?[ \t]*object[ \t]+([a-zA-Z0-9_]+)/\4/c,objects/
  ;; --regex-scala=/^[ \t]*((abstract|final|sealed|implicit|lazy)[ \t]*)*(private[^ ]*|protected)?[ \t]*((abstract|final|sealed|implicit|lazy)[ \t]*)*case class[ \t]+([a-zA-Z0-9_]+)/\6/c,case classes/
  ;; --regex-scala=/^[ \t]*((abstract|final|sealed|implicit|lazy)[ \t]*)*(private[^ ]*|protected)?[ \t]*case object[ \t]+([a-zA-Z0-9_]+)/\4/c,case objects/
  ;; --regex-scala=/^[ \t]*((abstract|final|sealed|implicit|lazy)[ \t]*)*(private[^ ]*|protected)?[ \t]*trait[ \t]+([a-zA-Z0-9_]+)/\4/t,traits/
  ;; --regex-scala=/^[ \t]*((@inline|@noinline|abstract|final|sealed|implicit|lazy|private[^ ]*(\[[a-z]*\])*|protected)[ \t]*)*def[ \t]+([a-zA-Z0-9_]+)/\4/m,methods/
  ;; --regex-scala=/^[ \t]*package[ \t]+([a-zA-Z0-9_.]+)/\1/p,packages/
  ;;
  ;; $ ctags --verbose -R -e
  (require 'helm-etags-plus)
  (setq helm-etags-plus-use-absolute-path nil)
  (set-face-foreground 'helm-etags-plus-file-face "green"))
