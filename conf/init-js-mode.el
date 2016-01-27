;;; js.el --- Major mode for editing JavaScript
(when (> (string-to-number emacs-version) 23.1) ;23.1以前には未対応
  (require 'js)
  (setq js-indent-level 2)
  )
