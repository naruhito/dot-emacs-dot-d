;;; web-search.el --- Open web pages using your default browser
(require 'web-search)
(require 'additional-web-services "~/.emacs.d/etc/additional-web-services")

(setq ws/search-engines
      (append (list
               '("find" . "https://www.google.com/bookmarks/find?hl=ja&q=%s")
               '("BGM" . "http://musictonic.com/music/%s")
               '("AOJ" . "http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=%s")
               '("tiny" . "http://tinyurl.com/create.php?source=indexpage&url=%s"))
              additional-search-engines
              ws/search-engines))

(setq ws/bookmarks
      (append (list)
              additional-bookmarks
              ws/bookmarks))
