;;; init-treesit-fold.el --- treesit-fold settings

;; treesit-fold がインストールされていれば読み込み、グローバルモードを有効化する
(when (require 'treesit-fold nil t)
  (global-treesit-fold-mode 1))

;; treesit-fold 用の org-mode 風折りたたみ設定
(defvar-local my/treesit-fold-all-state nil)
(defun my/treesit-fold-toggle-all ()
  "バッファ全体の折りたたみをトグルします。"
  (interactive)
  (when (fboundp 'treesit-fold-close-all)
    (if my/treesit-fold-all-state
        (progn
          (ignore-errors (treesit-fold-open-all))
          (setq my/treesit-fold-all-state nil))
      (progn
        (ignore-errors (treesit-fold-close-all))
        (setq my/treesit-fold-all-state t)))))

(provide 'init-treesit-fold)
;;; init-treesit-fold.el ends here
