;;; gist.el --- Emacs integration for gist.github.com
(require 'utilities "~/.emacs.d/utilities")
(add-to-load-path "~/.emacs.d/elisp/gist")
(add-to-load-path "~/.emacs.d/elisp/gh")
(add-to-load-path "~/.emacs.d/elisp/pcache")
(add-to-load-path "~/.emacs.d/elisp/logito")
(add-to-load-path "~/.emacs.d/elisp/tabulated-list")
(require 'gist)

;; 確認してから実行 (誤送信を防止)
(defadvice gist-region (around around-gist-region activate)
  (if (and (y-or-n-p "Gist? ")
           (y-or-n-p "Really?? "))
      ad-do-it))
