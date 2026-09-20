;;; init-treesit-auto.el --- treesit-auto settings

(when (require 'treesit-auto nil t)
  ;; 新しい言語のファイルを開いた時、パーサーをインストールするか確認プロンプトを出す
  ;; (完全に裏で自動インストールさせたい場合は 'prompt の代わりに t を指定します)
  (setq treesit-auto-install 'prompt)

  ;; global-treesit-auto-mode を有効にすると、自動で -ts-mode に切り替わるようになります
  (global-treesit-auto-mode))

(provide 'init-treesit-auto)
;;; init-treesit-auto.el ends here
