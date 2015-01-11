;;;; save-load-path.el --- save load-path and reuse it to test
(require 'save-load-path)
(save-load-path-initialize)
(setq save-load-path-file "~/.emacs.d/var/saved-load-path.el")
