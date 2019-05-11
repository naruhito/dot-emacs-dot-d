;;; markdown-mode.el --- Emacs Major mode for Markdown-formatted text files
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(custom-set-faces
 '(markdown-header-face ((((class color) (min-colors 88) (background dark)) (:foreground "#DB3D36" :weight bold))))
 '(markdown-header-rule-face ((t (:inherit markdown-header-face))))
 '(markdown-header-delimiter-face ((t (:inherit markdown-header-face))))
 '(markdown-link-face ((t (:inherit font-lock-builtin-face :foreground "#D68E31"))))
 '(markdown-pre-face ((t (:inherit font-lock-constant-face :foreground "#3BAF75"))))
 '(markdown-inline-code-face ((t (:inherit font-lock-constant-face :foreground "#3BAF75"))))
 '(markdown-list-face ((t (:inherit font-lock-builtin-face :foreground "#FBD01D" :weight bold))))
 )
