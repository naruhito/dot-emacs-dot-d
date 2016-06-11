;;;; init.el --- Initialize Emacs

;; Author: naruhito <naruhito78@gmail.com>
;; URL: https://github.com/naruhito/

;; Compatibility: GNU Emacs 23-24

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
(load "init-eshell")
(load "init-auto-save-buffers")
(load "init-paredit")
(load "init-lispxmp")
(load "init-open-junk-file")
(load "init-popwin")
(load "init-auto-complete")
(load "init-one-key")
(load "init-key-chord")
(load "init-anything")
(load "init-browse-kill-ring")
(load "init-word-count")
(load "init-session")
(load "init-skk")
(load "init-bm")
(load "init-yasnippet")
(load "init-kmacro")
(load "init-color-moccur")
(load "init-ispell")
(load "init-web-search")
(load "init-ediff")
(load "init-viewer")
(load "init-calendar")
(load "init-scss-mode")
(load "init-ruby-mode")
(load "init-web-mode")
(load "init-js-mode")
(load "init-markdown-mode")
(load "init-ensime")
(load "init-cmake-mode")
(load "init-git-gutter")
(load "init-helm")
(load "init-emacs")

;;; init.el ends here.
