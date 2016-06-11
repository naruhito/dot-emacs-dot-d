;;; web-search.el --- Open web pages using your default browser
;; -*- coding: utf-8; -*-

;; Copyright (C) 2009 Tomoya Otake <tomoya.ton@gmail.com>
;; Copyright (C) 2012 naruhito <naruhito78@gmail.com>

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

;;; Commentary:

;; This file was originally written by Tomoya Otake <tomoya.ton@gmail.com>,
;; then renamed and modified by naruhito <naruhito78@gmail.com>.
;; The original file can be downloaded at https://github.com/tomoya/search-web.el.

;;; Code:

(defvar default-search-engine "google ja")

(defvar ws/search-engines
  '(("twitter" . "https://twitter.com/search?q=%s")
    ("real" . "http://realtime.search.yahoo.co.jp/search?p=%s")
    ("AOJ" . "http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=%s")
    ("find" . "https://www.google.com/bookmarks/find?hl=ja&q=%s")
    ("alc" . "http://eow.alc.co.jp/%s/UTF-8/")
    ("amazon" . "http://www.amazon.com/s/url=search-alias%%3Daps&field-keywords=%s")
    ("amazon jp" . "http://www.amazon.co.jp/gp/search?index=blended&field-keywords=%s")
    ("google en" . "http://www.google.com/search?hl=en&q=%s")
    ("google ja" . "http://www.google.com/search?hl=ja&q=%s")
    ("google" . "http://www.google.com/search?q=%s")))

(defvar ws/bookmarks
  '(("gmail" . "http://gmail.com/")
    ("cal" . "https://www.google.com/calendar/")))

(defun ws/web-search (engine word)
  (if (stringp word)
      (kill-new word))
  (browse-url
   (format
    (cdr (assoc engine
                (append ws/search-engines ws/bookmarks)))
    (url-hexify-string word))))

(defun ws/make-search-engines-name-list ()
  (let ((result))
    (dolist (engine ws/search-engines)
      (add-to-list 'result (car engine)))
    result))

(defun ws/make-bookmarks-name-list ()
  (let ((result))
    (dolist (bookmark ws/bookmarks)
      (add-to-list 'result (car bookmark)))
    result))

(defun web-search-at-point ()
  (interactive)
  (let* ((completion-ignore-case t)
         (engine (completing-read "Search Engine: "
                                  (ws/make-search-engines-name-list) nil t)))
    (ws/web-search engine (substring-no-properties (thing-at-point 'word)))))

(defun web-search-region (&optional not-quoted)
  (interactive "P")
  (let* ((completion-ignore-case t)
         (beg (mark))
         (end (point))
         (engine (completing-read "Search Engine: "
                                  (ws/make-search-engines-name-list) nil t))
         (query (buffer-substring-no-properties beg end)))
    (if (not not-quoted)
        (setq query (concat "\"" query "\"")))
    (ws/web-search engine query)))

(defun web-search-query-default ()
  (interactive)
  (let ((query (read-string "Query: ")))
    (cond
     ((string-match "^http\\(s\\)?://" query)
      (kill-new query)
      (browse-url query)
      )
     ((string-match "^[a-zA-Z0-9]+[a-zA-Z0-9.]*[.]+[_a-zA-Z0-9/?=]+\\([.][_a-zA-Z0-9?=]+\\)?$" query)
      (kill-new query)
      (browse-url (concat "http://" query))
      )
     (t
      (ws/web-search default-search-engine query)
      ))))

(defun web-search-query ()
  (interactive)
  (let* ((completion-ignore-case t)
         (engine (completing-read
                  "Web Service: "
                  (append (ws/make-search-engines-name-list)
                          (ws/make-bookmarks-name-list)) nil t))
         query)
    (if (member engine (ws/make-search-engines-name-list))
        (setq query (read-string "Query: ")))
    (ws/web-search engine query)))

(provide 'web-search)

;;; web-search.el ends here
