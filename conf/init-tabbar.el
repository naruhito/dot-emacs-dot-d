;;; tabbar.el --- Display a tab bar in the header line
(require 'tabbar)

;; タブバーの有効化
(tabbar-mode)

;; 色の設定
;; アクティブ
(set-face-attribute 'tabbar-selected nil
                    :background "#ff5f00"
                    :foreground "black"
                    :box nil)
;; 非アクティブ
(set-face-attribute 'tabbar-unselected nil
                    :background "black"
                    :foreground "white"
                    :box nil)
;; アクティブ、変更が未保存
(set-face-attribute 'tabbar-selected-modified nil
                    :background "#ff5f00"
                    :foreground "yellow"
                    :box nil)
;; 非アクティブ、変更が未保存
(set-face-attribute 'tabbar-modified nil
                    :background "black"
                    :foreground "yellow"
                    :box nil)
;; セパレータ
(set-face-attribute 'tabbar-separator nil
                    :background "black"
                    :foreground "white"
                    :box nil)
;; その他の既定値
(set-face-attribute 'tabbar-default nil
                    :background "black"
                    :foreground "black"
                    :box nil)

;; 画像をボタンに利用しない
(setq tabbar-use-images nil)

;; ナビゲーションボタンを表示しない
(setq tabbar-buffer-home-button '(("") ""))
(setq tabbar-scroll-left-button '(("") ""))
(setq tabbar-scroll-right-button '(("") ""))

;; メジャーモード毎に分離されるタブグループを利用しない
(setq tabbar-buffer-groups-function nil)

;; タブのセパレータの長さ
(setq tabbar-separator '(2.0))

;; 表示するバッファの設定
(defun my-tabbar-buffer-list ()
  (delq nil
        (mapcar #'(lambda (b)
                    (cond
                     ;; カレントバッファはファイルでなくても含める
                     ((eq (current-buffer) b) b)
                     ;; バッファ内のファイルを含める
                     ((buffer-file-name b) b)
                     ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                     ;; "*eshell" で始まるバッファを含める
                     ((string-match (rx "*eshell" (* anything) "*") (buffer-name b)) b)
                     ;; それ以外の * で始まるバッファを含めない
                     ((char-equal ?* (aref (buffer-name b) 0)) nil)
                     ((buffer-live-p b) b)))
                (buffer-list))))
(setq tabbar-buffer-list-function 'my-tabbar-buffer-list)

;; 一つ前のバッファに移動する関数
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; 現在のタブを一つ左に移動
(defun tabbar-move-current-tab-one-place-left ()
  "Move current tab one place left, unless it's already the leftmost."
  (interactive)
  (let* ((bufset (tabbar-current-tabset t))
         (old-bufs (tabbar-tabs bufset))
         (first-buf (car old-bufs))
         (new-bufs (list)))
    (if (string= (buffer-name) (format "%s" (car first-buf)))
        old-bufs ; the current tab is the leftmost
      (setq not-yet-this-buf first-buf)
      (setq old-bufs (cdr old-bufs))
      (while (and
              old-bufs
              (not (string= (buffer-name) (format "%s" (car (car old-bufs))))))
        (push not-yet-this-buf new-bufs)
        (setq not-yet-this-buf (car old-bufs))
        (setq old-bufs (cdr old-bufs)))
      (if old-bufs ; if this is false, then the current tab's buffer name is mysteriously missing
          (progn
            (push (car old-bufs) new-bufs) ; this is the tab that was to be moved
            (push not-yet-this-buf new-bufs)
            (setq new-bufs (reverse new-bufs))
            (setq new-bufs (append new-bufs (cdr old-bufs))))
        (error "Error: current buffer's name was not found in Tabbar's buffer list."))
      (set bufset new-bufs)
      (tabbar-set-template bufset nil)
      (tabbar-display-update))))

;; 現在のタブを一つ右に移動
(defun tabbar-move-current-tab-one-place-right ()
  "Move current tab one place right, unless it's already the rightmost."
  (interactive)
  (let* ((bufset (tabbar-current-tabset t))
         (old-bufs (tabbar-tabs bufset))
         (first-buf (car old-bufs))
         (new-bufs (list)))
    (while (and
            old-bufs
            (not (string= (buffer-name) (format "%s" (car (car old-bufs))))))
      (push (car old-bufs) new-bufs)
      (setq old-bufs (cdr old-bufs)))
    (if old-bufs ; if this is false, then the current tab's buffer name is mysteriously missing
        (progn
          (setq the-buffer (car old-bufs))
          (setq old-bufs (cdr old-bufs))
          (if old-bufs ; if this is false, then the current tab is the rightmost
              (push (car old-bufs) new-bufs))
          (push the-buffer new-bufs)) ; this is the tab that was to be moved
      (error "Error: current buffer's name was not found in Tabbar's buffer list."))
    (setq new-bufs (reverse new-bufs))
    (setq new-bufs (append new-bufs (cdr old-bufs)))
    (set bufset new-bufs)
    (tabbar-set-template bufset nil)
    (tabbar-display-update)))
