;;; eglot.el --- The Emacs Client for LSP servers  -*- lexical-binding: t; -*-
(require 'eglot)

(with-eval-after-load 'eglot
  ;; Pythonモード時に、指定したディレクトリのpyright-langserverを起動するよう上書き
  (add-to-list 'eglot-server-programs
               `(python-mode . (,(expand-file-name "~/.emacs.d/.cache/eglot/node_modules/.bin/pyright-langserver") "--stdio"))))

;; 特定のモード（言語）を開いたときに自動で Eglot を起動する
(add-hook 'python-mode-hook 'eglot-ensure)  ;Python(pyright-langserver)
(add-hook 'web-mode-hook 'eglot-ensure)  ;TypeScript(ts-ls), JavaScript, HTML, CSS
(add-hook 'go-mode 'eglot-ensure)  ;Go
(add-hook 'c-mode-hook 'eglot-ensure)  ;C/C++
(add-hook 'java-mode-hook 'eglot-ensure)  ;Java
