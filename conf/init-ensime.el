;;; ensime.el --- ENhanced Scala Interaction Mode for Emacs
;; ensime-server を http://ensime.typelevel.org/ から取得して ~/.emacs.d/ensime/ に保存
;; 参考:  http://ensime.github.io/editors/emacs/install/#installing-the-server-from-assembly-builds-or-source
;;        > SNAPSHOT assembly jars are provided at http://ensime.typelevel.org/

(require 'utilities "~/.emacs.d/utilities")
(add-to-load-path "~/.emacs.d/elisp/ensime")

(when (<= 24 emacs-major-version) ;23以前には未対応
  (require 'scala-mode2)
  (require 'auto-complete)
  (require 'ensime)
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

  ;; ensime の補完に auto-complete を使用する
  (setq ensime-completion-style 'auto-complete)

  ;; ドット '.' を補完機能のトリガーに設定
  (defun scala/completing-dot ()
    "Insert a period and show company completions."
    (interactive "*")
    (eval-and-compile (require 'ensime))
    (eval-and-compile (require 's))
    (when (s-matches? (rx (+ (not space)))
                      (buffer-substring (line-beginning-position) (point)))
      (delete-horizontal-space t))
    (cond ((not (and (ensime-connected-p) ensime-completion-style))
           (insert "."))
          ((eq ensime-completion-style 'auto-complete)
           (insert ".")
           (ac-trigger-key-command t))))
  (define-key scala-mode-map (kbd ".") 'scala/completing-dot))
