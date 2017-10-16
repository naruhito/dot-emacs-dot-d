;;; yaml-mode.el --- Major mode for editing YAML files
(when (<= 24 emacs-major-version) ;23以前には未対応
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))
