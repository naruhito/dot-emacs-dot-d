;;; ispell.el --- interface to International Ispell Versions 3.1 and 3.2
;; ` ': Accept word this time. (SPC)
;; `i': Accept word and insert into private dictionary.
(require 'ispell)                       ; 動作要件: aspell を brew や yum でインストール
(setq ispell-program-name "aspell")
(setq ispell-personal-dictionary "~/.emacs.d/var/aspell-pdict")
