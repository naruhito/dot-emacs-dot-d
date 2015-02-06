;;; web-search.el --- Open web pages using your default browser
(require 'web-search)
(require 'web-bookmarks "~/.emacs.d/etc/web-bookmarks")

(setq ws/search-engines
      (append search-engines ws/search-engines))

(setq ws/bookmarks
      (append bookmarks ws/bookmarks))
