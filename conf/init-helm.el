;;; helm.el --- Emacs incremental and narrowing framework -*- lexical-binding: t -*-
(when (<= 24 emacs-major-version) ;23以前には未対応
  (require 'utilities "~/.emacs.d/utilities")
  (add-to-load-path "~/.emacs.d/elisp/emacs-async")
  (add-to-load-path "~/.emacs.d/elisp/helm")
  (require 'helm-config))
