;;; anzu.el --- Show number of matches in mode-line while searching

(when (<= 24 emacs-major-version)
  (require 'anzu)
  (global-anzu-mode +1))
