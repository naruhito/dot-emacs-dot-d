;;; scala-mode2.el --- Major mode for editing scala
(when (<= 24 emacs-major-version) ;23以前には未対応
  (require 'utilities "~/.emacs.d/utilities")
  (add-to-load-path "~/.emacs.d/elisp/scala-mode2")
  (require 'scala-mode2)
  )
