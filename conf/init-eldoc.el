;;; eldoc.el --- show function arglist or variable docstring in echo area
(require 'eldoc)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; すぐに表示
(setq eldoc-idle-delay 0.1)

;; モードラインに表示しない
(setq eldoc-minor-mode-string "")
