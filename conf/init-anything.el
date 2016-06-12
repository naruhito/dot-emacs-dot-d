;;; anything-startup.el --- anything.el startup file
(when (> 24 emacs-major-version) ;24より前のバージョンでのみ使用
  (require 'utilities "~/.emacs.d/utilities")
  (add-to-load-path "~/.emacs.d/elisp/anything")
  (require 'anything-startup)

  ;; *scratch*, *eshell* 等の、ファイルと関連付けられていないバッファを候補から除外 (必要な場合、switch-to-buffer)
  (setq anything-c-boring-buffer-regexp
        (rx (or (group "*" (+ not-newline) "*")
                (group (+ not-newline) ".erb-template-indent-buffer"))))

  ;; カレントバッファを候補から除外
  (setq anything-allow-skipping-current-buffer t))
