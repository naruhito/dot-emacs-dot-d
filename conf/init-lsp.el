;;; lsp-mode.el --- LSP mode

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook
  (go-mode . lsp-deferred)
  (web-mode . lsp-deferred))  ; note: select "ts-ls" for typescript.

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

;; Python
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))

;; https://github.com/emacs-lsp/lsp-pyright?tab=readme-ov-file#choosing-the-correct-version-of-python
;; https://docs.python.org/ja/3/library/venv.html#creating-virtual-environments
(when (eq system-type 'windows-nt)
  (defun lsp-pyright--locate-python-venv ()
    "Find a python executable based on the current virtual environment."
    (executable-find (f-expand "Scripts/python.exe" (lsp-pyright--locate-venv)))))

;; Go
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Java
(use-package lsp-java
  :config (add-hook 'java-mode-hook 'lsp))
