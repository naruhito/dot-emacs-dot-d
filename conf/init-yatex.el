;;;; Yet Another tex-mode for emacs - //野鳥//
(require 'utilities "~/.emacs.d/utilities")
(add-to-load-path "~/.emacs.d/elisp/yatex")
(add-hook 'yatex-mode-hook 'abbrev-mode)
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
