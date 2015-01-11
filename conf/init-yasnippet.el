;;; Yasnippet.el --- Yet another snippet extension for Emacs.
(require 'utilities "~/.emacs.d/utilities")
(add-to-load-path "~/.emacs.d/elisp/yasnippet")
(require 'yasnippet-config)
(yas/initialize)
(setq yas/root-directory
      '("~/.emacs.d/snippets"
        "~/.emacs.d/elisp/yasnippet/snippets"
        ))
(mapc 'yas/load-directory yas/root-directory)
