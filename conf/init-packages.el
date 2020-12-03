(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(package-refresh-contents)


(defvar my/favorite-packages
  '(
    anzu
    bm
    browse-kill-ring
    cmake-mode
    color-moccur
    company
    company-lsp
    ddskk
    flycheck
    git-gutter
    go-mode
    groovy-mode
    helm
    helm-bm
    helm-etags-plus
    key-chord
    lispxmp
    lsp-mode
    markdown-mode+
    open-junk-file
    paredit
    popup
    qml-mode
    real-auto-save
    sbt-mode
    scala-mode
    scss-mode
    session
    tabbar
    viewer
    web-mode
    yaml-mode
    yasnippet
    popwin
    lsp-ui
    use-package
    lsp-pyright
    ))

(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred
