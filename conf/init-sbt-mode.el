;;; sbt-mode.el --- Functions for discovering the current sbt project
(when (<= 24 emacs-major-version) ;23以前には未対応
  (require 'utilities "~/.emacs.d/utilities")
  (add-to-load-path "~/.emacs.d/elisp/sbt-mode")
  (require 'sbt-mode)
  )
