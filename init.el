;;;; init.el --- Initialize Emacs

;; Author: naruhito <naruhito78@gmail.com>
;; URL: https://github.com/naruhito/dot-emacs-dot-d

;; Compatibility: GNU Emacs 30

;;; License

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; Set environment variables.
(require 'set-env-vars "~/.emacs.d/etc/set-env-vars")

;; Define utility functions.
(require 'utilities "~/.emacs.d/utilities")

;; Add "elisp/" and "conf/" to load-path.
(add-to-load-path "~/.emacs.d/elisp"
                  "~/.emacs.d/conf")

;; Load init files.
(load "init-packages")
(load "init-lsp")
(load "init-eshell")
(load "init-real-auto-save")
(load "init-paredit")
(load "init-lispxmp")
(load "init-open-junk-file")
(load "init-key-chord")
(load "init-browse-kill-ring")
(load "init-word-count")
(load "init-session")
(load "init-skk")
(load "init-bm")
(load "init-yasnippet")
(load "init-kmacro")
(load "init-ispell")
(load "init-web-search")
(load "init-viewer")
(load "init-calendar")
(load "init-ruby-mode")
(load "init-web-mode")
(load "init-markdown-mode")
(load "init-cmake-mode")
(load "init-git-gutter")
(load "init-helm")
(load "init-company-mode")
(load "init-flycheck")
(load "init-groovy-mode")
(load "init-yaml-mode")
(load "init-anzu")
(load "init-qml-mode")
(load "init-go-mode")
(load "init-tabbar")
(load "init-plantuml-mode")
(load "init-terraform-mode")
(load "init-emojify-mode")
(load "init-copilot")
(load "init-emacs")

;;; init.el ends here.
