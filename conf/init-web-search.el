;;; web-search.el --- Open web pages using your default browser
(require 'web-search)
(require 'web-bookmarks "~/.emacs.d/etc/web-bookmarks")

(setq ws/search-engines
      (append (list
               '("find" . "https://www.google.com/bookmarks/find?hl=ja&q=%s")
               '("tiny" . "http://tinyurl.com/create.php?source=indexpage&url=%s")
               '("AOJ" . "http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=%s")
               '("BGM" . "http://musictonic.com/music/%s")
               )
              search-engines ws/search-engines))

(setq ws/bookmarks
      (append (list) bookmarks ws/bookmarks))
