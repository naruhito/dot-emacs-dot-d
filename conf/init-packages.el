;;; package.el --- Simple package system for Emacs  -*- lexical-binding:t -*-
(require 'package)

;; レポジトリの追加
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; パッケージ情報の初期化
(package-initialize)
(package-refresh-contents)

;; インストール対象のパッケージ一覧
(defvar my/target-packages
  '(
    anzu
    bm
    browse-kill-ring
    cmake-mode
    color-moccur
    company
    ddskk
    emojify
    flycheck
    flycheck-popup-tip
    git-gutter
    go-mode
    groovy-mode
    helm
    helm-bm
    key-chord
    lispxmp
    lsp-mode
    lsp-java
    lsp-pyright
    lsp-ui
    open-junk-file
    paredit
    plantuml-mode
    popup
    popwin
    qml-mode
    real-auto-save
    sbt-mode
    scala-mode
    scss-mode
    session
    tabbar
    terraform-mode
    treesit-auto
    use-package
    viewer
    web-mode
    yaml-mode
    yasnippet
    ))

;; インストールの実行
(dolist (package my/target-packages)
  (unless (package-installed-p package)
    (package-install package)))
