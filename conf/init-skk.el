;;;; skk.el --- Daredevil SKK (Simple Kana to Kanji conversion program) *詳しくは skk/doc/skk-14.4.pdf または info を参照
(require 'utilities "~/.emacs.d/utilities")
(add-to-load-path "~/.emacs.d/elisp/skk")
(require 'skk-autoloads)

;; SKK設定ファイルの置き場所
(defvar skk-user-directory "~/.emacs.d/skk")
(require 'skk-vars)

;; チュートリアルの置き場所
(setq skk-tut-file "~/.emacs.d/skk/tutorial/SKK.tut")

;; 小指が疲れないようにするための設定
(setq skk-sticky-key ";")

;; YaTeX のときだけ句読点を変更したい
(add-hook 'yatex-mode-hook
          (lambda ()
            (require 'skk)
            (setq skk-kutouten-type '(". " . ", "))))

;; SKK Openlabから頂いた辞書を登録
(setq skk-large-jisyo "~/.emacs.d/skk/SKK-JISYO.L")
(setq skk-extra-jisyo-file-list
      (mapcar
       '(lambda (jisyo)
          (concat "~/.emacs.d/skk/" jisyo))
       '("SKK-JISYO.jinmei"
         "SKK-JISYO.fullname"
         "SKK-JISYO.geo"
         "SKK-JISYO.propernoun"
         "SKK-JISYO.station"
         "SKK-JISYO.law")))

;; Emacs起動時に、辞書を読み込む
(setq skk-preload t)

;; 動的補完を有効にする
(setq skk-dcomp-activate t)

;; 動的補完の候補を複数表示する
(setq skk-dcomp-multiple-activate t)

;; 動的補完候補の表示数
(setq skk-dcomp-multiple-rows 7)

;; カタカナ語の変換結果も辞書に登録する
(setq skk-search-katakana t)

;; ファイルを開いた時、skk-latin-modeを有効にする
(require 'skk-vars)
(require 'skk-macs)
(require 'skk)
(add-hook 'find-file-hooks 'skk-mode-on-with-latin)
(add-hook 'after-revert-hook 'skk-mode-on-with-latin)
(add-hook 'eshell-mode-hook 'skk-mode-on-with-latin)
(defun skk-mode-on-with-latin ()
  (interactive)
  (skk-mode)
  (skk-kakutei)
  (skk-latin-mode-on))

;; Infoディレクトリを追加
(add-to-list 'Info-default-directory-list "~/.emacs.d/skk/info")
