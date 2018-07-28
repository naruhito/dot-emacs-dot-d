;;; eclim.el --- an interface to the Eclipse IDE ( https://github.com/emacs-eclim/emacs-eclim )

;; インストール手順

;; 1. Eclipse をインストール https://www.eclipse.org/downloads/packages/

;; 2. http://eclim.org/install.html にしたがって eclim をインストール
;; - Graphical Installer
;; - Do not install vim files (Emacs users only) にチェック
;; - Java Development にチェック
;; - Headed eclipse server を起動

;; 使い方

;; eclim-manage-projects
;; - プロジェクト一覧を表示

;; eclim-problems (C-c C-e b)
;; - エラーおよび警告一覧
;; - "e" "w" で error/warning の表示
;; - "g" でリフレッシュ
;; - C-m で該当箇所に移動
;; - "q" で閉じる

;; eclim-java-import-organize (C-c C-e i)
;; - import 文を整理

;; eclim-java-find-declaration (C-c C-e f d)
;; - 定義箇所に移動

;; eclim-java-implement (C-c C-e z)
;; - 編集中のクラスのメソッドを実装するための scaffold を生成

;; eclim-java-hierarchy (C-c C-e h)
;; - 現在の位置

;; eclim-complete (C-M-i)
;; - 補完候補を強制表示

;; eclim-java-generate-getter-and-setter (C-c C-e g)
;; - カーソルの箇所のメンバ変数の getter と setter を生成

(when (and
       (= 24 emacs-major-version)
       (<= 5 emacs-minor-version)) ;24.5 以降のみ対応
  (require 'utilities "~/.emacs.d/utilities")
  (add-to-load-path "~/.emacs.d/elisp/eclim")
  (require 'eclim)

  ;; eclimd を自動では起動しません。
  ;; http://eclim.org/install.html に記載の手順で手動起動します。
  ;; Window > Show View > Other > Eclim > eclimd
  (setq eclimd-autostart nil)

  ;; 設定値がある場合は上書き
  (when eclipse-dir
    (setq eclim-eclipse-dirs (list eclipse-dir))
    (setq eclim-executable (concat eclipse-dir "/eclim")))

  ;; カーソルのある箇所に関するエラーメッセージを echo エリアに表示します。
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)

  ;; 補完設定
  (require 'company)
  (require 'company-emacs-eclim)
  (company-emacs-eclim-setup)
  (setq company-emacs-eclim-ignore-case t) ;大文字小文字を区別しない。
  (global-company-mode t))

;; Java インデント設定
(add-hook 'java-mode-hook
          (lambda ()
            (message "java-mode-hook")
            (setq tab-width java-indent-width)
            (setq indent-tabs-mode java-indent-tab)
            (setq c-basic-offset java-indent-width)))
