;;; session.el --- use variables, registers and buffer places across sessions
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(setq session-save-file
      (expand-file-name "~/.emacs.d/var/session"))
