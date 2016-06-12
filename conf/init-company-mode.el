;;; company.el --- Modular text completion framework

;; キーバインド

;; - C-s   候補内でインクリメンタルサーチ
;; - C-M-s 候補内でインクリメンタルサーチ + フィルタリング
;; - TAB   全候補に共通する prefix 部分を挿入
;; - C-w   候補を定義しているファイルを開いて表示

(when (<= 24 emacs-major-version) ;23以前には未対応
  (require 'utilities "~/.emacs.d/utilities")
  (add-to-load-path "~/.emacs.d/elisp/company-mode")
  (require 'company)

  ;; 全バッファーで有効化 (eshell を除く)
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'eshell-mode-hook (lambda () (company-mode -1)))

  ;; 候補の表示までの待ち時間を 0 にする
  (setq company-idle-delay 0)

  ;; 色設定
  (set-face-attribute 'company-tooltip nil :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common nil :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common-selection nil :foreground "white" :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil :foreground "black" :background "steelblue")
  (set-face-attribute 'company-preview-common nil :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil :background "orange")
  (set-face-attribute 'company-scrollbar-bg nil :background "gray40"))
