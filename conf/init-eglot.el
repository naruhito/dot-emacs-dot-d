;;; eglot.el --- The Emacs Client for LSP servers  -*- lexical-binding: t; -*-
(require 'eglot)

;; pyright-langserver
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(python-mode . (,(expand-file-name "~/.emacs.d/.cache/eglot/node_modules/.bin/pyright-langserver") "--stdio"))))

;; tsc (typescript7)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `((typescript-mode typescript-ts-mode web-mode)
                 . (,(expand-file-name "~/.emacs.d/.cache/eglot/node_modules/.bin/tsc") 
                    "--lsp" "--stdio"))))

;; 特定のモード（言語）を開いたときに自動で Eglot を起動する
(add-hook 'python-mode-hook 'eglot-ensure)  ;Python(pyright-langserver)
(add-hook 'web-mode-hook 'eglot-ensure)  ;TypeScript(tsc)
(add-hook 'c-mode-hook 'eglot-ensure)  ;C/C++
