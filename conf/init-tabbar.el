;;; tabbar.el --- Display a tab bar in the header line
(require 'tabbar)
(tabbar-mode)

;; マウスホイール無効
(tabbar-mwheel-mode nil)

;; 画像を利用しない
(setq tabbar-use-images nil)

;; ボタンを表示しない
(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))

;; タブのセパレータの長さ
(setq tabbar-separator '(2.0))

;; タブの色
(set-face-attribute 'tabbar-default nil
                    :background "brightblue"
                    :foreground "white")
(set-face-attribute 'tabbar-selected nil
                    :background "#ff5f00"
                    :foreground "brightwhite"
                    :box nil)
(set-face-attribute 'tabbar-modified nil
                    :background "brightred"
                    :foreground "brightwhite"
                    :box nil)

;; タブグループの無効化
(setq tabbar-buffer-groups-function nil)

;; 表示するバッファの設定
(defun my-tabbar-buffer-list ()
  (delq nil
        (mapcar #'(lambda (b)
                    (cond
                     ;; Always include the current buffer.
                     ((eq (current-buffer) b) b)
                     ((buffer-file-name b) b)
                     ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                     ((equal "*scratch*" (buffer-name b)) b) ; *scratch*バッファは表示する
                     ((char-equal ?* (aref (buffer-name b) 0)) nil) ; それ以外の * で始まるバッファは表示しない
                     ((buffer-live-p b) b)))
                (buffer-list))))
(setq tabbar-buffer-list-function 'my-tabbar-buffer-list)
