;;; init-emacs.el --- Emacs Settings

;; 拡張子とメジャーモードの関連づけ
(setq auto-mode-alist
      (append
       (list
        '("\\.gradle$" . groovy-mode)
        '("\\.erb$" . web-mode)
        '("\\.html$" . web-mode)
        '("Dockerfile$" . conf-mode)
        '("file$" . ruby-mode)
        '("\\.json$" . web-mode)
        '("\\.md$" . markdown-mode)
        '("CMakeLists.txt$" . cmake-mode)
        '("\\.txt$" . markdown-mode)
        '("\\.qml$" . qml-mode)
        '("\\.jsx$" . web-mode)
        '("\\.js$" . js-mode)
        '("\\.mjs$" . js-mode)
        '("\\.tsx$" . web-mode)
        '("\\.ts$" . web-mode)
        '("\\.go$" . go-mode)
        '("\\.pu$" . plantuml-mode)
        ) auto-mode-alist))

;; キーバインディングの設定
(defmacro defkey (keymap key command)
  `(define-key ,keymap ,(read-kbd-macro key) ,command))
(defmacro gdefkey (key command)
  `(define-key global-map ,(read-kbd-macro key) ,command))

(defkey view-mode-map "h" 'backward-char)
(defkey view-mode-map "j" 'next-line)
(defkey view-mode-map "k" 'previous-line)
(defkey view-mode-map "l" 'forward-char)
(defkey view-mode-map "J" 'View-scroll-line-forward)
(defkey view-mode-map "K" 'View-scroll-line-backward)
(defkey view-mode-map "f" 'View-scroll-page-forward)
(defkey view-mode-map "b" 'View-scroll-page-backward)
(defkey view-mode-map "C-d" 'View-scroll-half-page-forward)
(defkey view-mode-map "C-u" 'View-scroll-half-page-backward)
(defkey view-mode-map "G" 'View-goto-line-last)
(defkey view-mode-map "0" 'move-beginning-of-line)
(defkey view-mode-map "$" 'move-end-of-line)
(defkey view-mode-map "i" 'view-mode)
(defkey view-mode-map "q" '(lambda () (interactive)
                             (kill-buffer (current-buffer))))
(defkey view-mode-map "SPC" 'bm-toggle)
(defkey view-mode-map "[" 'bm-previous)
(defkey view-mode-map "]" 'bm-next)
(defkey company-active-map "M-n" nil)
(defkey company-active-map "M-p" nil)
(defkey company-active-map "C-p" 'company-select-previous)
(defkey company-active-map "C-n" 'company-select-next)
(defkey company-active-map "C-h" nil)
(defkey flycheck-mode-map "C-c C-p" 'flycheck-previous-error)
(defkey flycheck-mode-map "C-c C-n" 'flycheck-next-error)
(defkey helm-map "C-h" 'delete-backward-char)
(defkey helm-find-files-map "TAB" 'helm-execute-persistent-action)
(defkey yas-minor-mode-map "<C-S-i>" 'yas-expand-from-trigger-key)
(defkey emacs-lisp-mode-map "C-c C-d" 'lispxmp)
(defkey copilot-completion-map "<tab>" 'copilot-accept-completion)
(defkey copilot-completion-map "TAB" 'copilot-accept-completion)
(defkey copilot-completion-map "M-f" 'copilot-accept-completion-by-word)
(defkey copilot-completion-map "M-n" 'copilot-next-completion)
(defkey copilot-completion-map "M-p" 'copilot-previous-completion)
(gdefkey "M-k" 'copilot-complete)
(gdefkey "M-SPC" 'bm-toggle)
(gdefkey "M-[" 'bm-previous)
(gdefkey "M-]" 'bm-next)
(gdefkey "C-x p" 'git-gutter:previous-hunk)
(gdefkey "C-x n" 'git-gutter:next-hunk)
(gdefkey "C-x i" 'git-gutter/custom:toggle-popup-hunk)
(gdefkey "C-x h" 'git-gutter:stage-hunk)
(gdefkey "C-x C-f" 'helm-find-files)
(gdefkey "C-h" 'delete-backward-char)
(gdefkey "M-h" 'backward-delete-word)
(gdefkey "M-d" 'delete-word)
(gdefkey "M-x" 'helm-M-x)
(gdefkey "C-m" 'newline-and-indent)
(gdefkey "C-x RET" 'switch-to-buffer)
(gdefkey "C-M-y" 'browse-kill-ring)
(gdefkey "M-+" 'word-count-mode)
(gdefkey "C-x C-z" 'open-junk-file)
(gdefkey "C-x C-j" 'skk-mode)
(gdefkey "C-c r" 'anzu-query-replace-regexp)
(gdefkey "C-c C-r" 'window-resizer)
(gdefkey "C-x r i" 'string-insert-rectangle)
(gdefkey "C-x C-l" 'set-buffer-file-coding-system-utf-8-unix)
(gdefkey "C-x C-c" 'kill-other-file-buffers)
(gdefkey "C-t" nil)
(gdefkey "C-t C-t" 'switch-to-previous-buffer)
(gdefkey "C-t C-p" 'tabbar-move-current-tab-one-place-left)
(gdefkey "C-t C-n" 'tabbar-move-current-tab-one-place-right)
(gdefkey "C-t ," 'rename-buffer)
(gdefkey "C-t k" 'kill-current-buffer)
(gdefkey "C-t c" '(lambda () (interactive) (eshell (format-time-string "%s" (current-time)))))
(gdefkey "C-t n" 'tabbar-forward-tab)
(gdefkey "C-t p" 'tabbar-backward-tab)
(gdefkey "C-z" nil)
(gdefkey "C-z C-b" 'helm-imenu)
(gdefkey "C-z C-d" 'yas-new-snippet)
(gdefkey "C-z C-f" '(lambda () (interactive (set-frame-parameter nil 'fullscreen 'maximized))))
(gdefkey "C-z C-i" 'eshell)
(gdefkey "C-z C-j" '(lambda (arg) (interactive (list (thing-at-point 'symbol))) (helm-apropos arg)))
(gdefkey "C-z C-k" 'web-search-query-default)
(gdefkey "C-z C-l" 'web-search-query)
(gdefkey "C-z C-o" '(lambda () (interactive) (helm-buffers-list)))
(gdefkey "C-z C-p" 'helm-recentf)
(gdefkey "C-z C-s" 'kmacro-save)
(gdefkey "C-z C-t" 'describe-mode)
(gdefkey "C-z C-v" 'apropos-value)
(gdefkey "C-z C-w" 'describe-key)
(gdefkey "C-z C-x" 'revert-buffer-with-coding-system-utf-8-unix)
(gdefkey "C-z C-z" 'suspend-frame)
(gdefkey "C-z C-[" '(lambda () (interactive) (helm-bm)))
(gdefkey "C-z c" 'list-faces-display)
(gdefkey "C-z f" 'describe-face-at-point)
(gdefkey "C-z i" 'ispell)
(gdefkey "C-z j" 'browse-url-at-point)
(gdefkey "C-z k" 'web-search-region)
(gdefkey "C-z l" 'kmacro-end-or-call-macro)
(gdefkey "C-z n" 'create-new-junk-file)
(gdefkey "C-z o" 're-open-junk-file-at-point)
(gdefkey "C-z p" 'web-search-at-point)
(gdefkey "C-z q" 'save-buffers-kill-emacs)
(gdefkey "C-z x" 'revert-buffer-with-coding-system-utf-8-dos)
(gdefkey "C-z w" 'copy-current-junk-filename)
(gdefkey "C-z C-c" 'copilot-chat-display)
(gdefkey "C-z C-e a" 'copilot-chat-explain)
(gdefkey "C-z C-e f" 'copilot-chat-explain-defun)
(gdefkey "C-z C-e s" 'copilot-chat-explain-symbol-at-line)
(gdefkey "C-z C-r a" 'copilot-chat-review-whole-buffer)
(gdefkey "C-z C-r s" 'copilot-chat-review)
(gdefkey "C-z C-a" 'copilot-chat-custom-prompt-function)
(gdefkey "<C-tab>" 'tabbar-forward-tab)
(gdefkey "<C-S-tab>" 'tabbar-backward-tab)
(gdefkey "<C-iso-lefttab>" 'tabbar-backward-tab)
(gdefkey "<C-return>" 'highlight-symbol)
(gdefkey "<C-S-return>" 'highlight-symbol-query-replace)
(gdefkey "C-," 'highlight-symbol-prev)
(gdefkey "C-." 'highlight-symbol-next)

(add-hook 'eshell-mode-hook
          '(lambda ()
             (defkey eshell-mode-map "C-a" 'eshell-bol)
             (defkey eshell-mode-map "C-M-p" 'eshell-previous-matching-input-from-input)
             (defkey eshell-mode-map "C-M-n" 'eshell-next-matching-input-from-input)
             ))

(key-chord-mode 1)
(key-chord-define-global "jk" 'view-mode)
(key-chord-define-global "iw" 'ispell-word)

;; 言語を日本語に設定
(add-hook 'set-language-environment-hook
          '(lambda ()
             (cond ((eq system-type 'windows-nt)
                    ;; プロセス出力の decoding と入力の encoding
                    (setq default-process-coding-system '(utf-8 . japanese-shift-jis-unix))
                    ;; ファイル名の encode/decode で使用する符号化方式
                    (setq default-file-name-coding-system 'japanese-shift-jis)
                    ;; Fix for Emacs 29.2
                    (setq locale-coding-system 'japanese-shift-jis)
                    )
                   (t                   ;Windows以外の環境はすべて utf-8 で統一
                    (setq default-process-coding-system '(utf-8 . utf-8))
                    (setq default-file-name-coding-system 'utf-8)
                    (setq locale-coding-system 'utf-8)
                    ))))
(set-language-environment "Japanese")

;; kill-wordと異なり削除したwordをキルリングに保存しない
(defun delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

;; backward-kill-wordと異なり削除したwordをキルリングに保存しない
(defun backward-delete-word (arg)
  (interactive "p")
  (delete-word (- arg)))

;; 現在のファイルを "utf-8-unix" に変換する (例えば shift_jis-dos のファイルを utf-8-unix のファイルに変換したい場合に使用)
(defun set-buffer-file-coding-system-utf-8-unix ()
  (interactive)
  (if (and (not (coding-system-equal buffer-file-coding-system 'utf-8-unix))
           (not (coding-system-equal buffer-file-coding-system 'undecided-unix))
           (y-or-n-p "Set buffer-file-coding-system as utf-8-unix? "))
      (set-buffer-file-coding-system 'utf-8-unix)))

;; ファイルを開いたときなどに "utf-8-unix" でなければ変換するか尋ねる
(add-hook 'find-file-hooks 'set-buffer-file-coding-system-utf-8-unix)
(add-hook 'after-revert-hook 'set-buffer-file-coding-system-utf-8-unix)

;; エディタが誤認識した場合等に使用。"utf-8-unix/dos"で開きなおす (例えば utf-8-unix のファイルが shift_jis-dos として開かれた場合に使用)
(defun revert-buffer-with-coding-system-utf-8-unix ()
  (interactive)
  (if (not (coding-system-equal buffer-file-coding-system 'utf-8-unix))
      (revert-buffer-with-coding-system 'utf-8-unix)))

(defun revert-buffer-with-coding-system-utf-8-dos ()
  (interactive)
  (if (not (coding-system-equal buffer-file-coding-system 'utf-8-dos))
      (revert-buffer-with-coding-system 'utf-8-dos)))

;; 現在開いているバッファー以外を閉じる
(defun kill-other-file-buffers ()
  (interactive)
  (mapc
   (lambda (x)
     (if (not (string-match (rx "*" (+ anything) "*") (buffer-name x)))
         (kill-buffer x)))
   (delq (current-buffer) (buffer-list))))

;; 起動時のカレントディレクトリをホームディレクトリに設定
(cd "~")

;; エラーが起きてもデバッガが立ち上がらないようにする
(setq debug-on-error nil)

;; 起動時にウィンンドウ分割して表示される通知を隠す
(delete-other-windows)

;; 画面分割時に長い行を改行して表示する
(add-hook 'text-mode-hook #'visual-line-mode)

;; recentfの保存場所を変更
(setq recentf-save-file (convert-standard-filename "~/.emacs.d/var/recentf"))

;; BackUpFileを作らない
(setq make-backup-files nil)

;; Lock fileを作らない
(setq create-lockfiles nil)

;; 自動保存用ファイル (#*#) も作らない
(setq auto-save-default nil)
(setq auto-save-list-file-name "~/.emacs.d/var/auto-save-list")
(setq auto-save-list-file-prefix nil)

;; 静的略語定義を保存しない
(setq save-abbrevs nil)

;; インデントではモードを問わず空白を挿入する
(setq-default tab-width 4 indent-tabs-mode nil)
(setq c-basic-offset 4)
(setq sh-basic-offset 2)
(setq sh-indentation 2)
(setq smie-indent-basic 2)

;; upcase-word の対象を直前の単語に変更
(defadvice upcase-word (before before-upcase-word)
  (backward-word))
(ad-activate 'upcase-word)

;; ファイル名やバッファ名の補完で大文字小文字を区別しない
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; upcase-region と downcase-region を利用できるように有効化
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; 対応する括弧を強調表示
(show-paren-mode t)

;; status-barにカーソルのcolumn表示
(column-number-mode t)

;; アンダーライン表示
(global-hl-line-mode)
(setq hl-line-face 'underline)

;; 長い行を折り返すときのバックスラッシュを半角スペースに変更
(set-display-table-slot standard-display-table 'wrap 0)

;; 起動時に"*GNU Emacs*"バッファを作成しない
(setq inhibit-startup-message t)

;; 起動時に"*scratch*"バッファにメッセージを表示しない
(setq initial-scratch-message nil)

;; yes-noをy-nに置き換え
(fset 'yes-or-no-p 'y-or-n-p) 

;; ビープ音、画面フラッシュなし
(setq ring-bell-function 'ignore)

;; フェイスの設定値を一時的に書き出すファイルを指定 (list-faces-display)
(setq custom-file "~/.emacs.d/var/customize-face-tmp.el")

;; カーソル位置のフェイスを調査する関数
(defun describe-face-at-point ()
  (interactive)
  (message "%s" (get-text-property (point) 'face)))

;; 分割したウィンドウのサイズを hjkl で変更する関数
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action
              (read-key-sequence-vector (format "size[%dx%d]"
                                                (window-width)
                                                (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (let ((last-command-event (aref action 0))
                     (command (key-binding action)))
                 (when command
                   (call-interactively command)))
               (message "Quit")
               (throw 'end-flag t)))))))

;; 画面サイズ・フォント・色・その他
(cond
 ;;;;;;;;;;;;;;;;;;;; WINDOWS ;;;;;;;;;;;;;;;;;;;;
 ((eq system-type 'windows-nt)

  ;; マシンスペックに応じて設定。処理を軽くする。
  (setq gc-cons-threshold (* gc-cons-threshold 10))

  ;; メニューバーを非表示に設定
  (menu-bar-mode -1)

  ;; ツールバーを非表示に設定
  (tool-bar-mode -1)

  ;; スクロールバーを非表示に設定
  (scroll-bar-mode -1)

  ;; 編集中ファイルのフルパスをタイトルバーに表示する
  (setq frame-title-format
        (format "%%f  Emacs@%s" (system-name)))

  ;; 透明度の設定
  (add-to-list 'default-frame-alist
               '(alpha . 80))

  ;; 選択時の色 (文字: Black, 背景: PaleGreen)
  (set-face-foreground 'region "Black")
  (set-face-background 'region "PaleGreen")

  ;; 通常時の色 (文字: White, 背景: Black, カーソル: Gray)
  (set-foreground-color "White")
  (set-background-color "Black")
  (set-cursor-color "Gray")

  ;; デフォルトのフォント設定など (フォントセット)
  (set-face-attribute 'default nil
                      :height 130)

  ;; フォントセットのうち、日本語のフォントだけメイリオに変更
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    '("メイリオ" . "unicode-bmp"))

  (set-fontset-font (frame-parameter nil 'font)
                    'katakana-jisx0201
                    '("メイリオ" . "unicode-bmp"))

  ;; 特定のフォントについて、文字幅を調整 (半角英数：日本語=1：2)
  (setq face-font-rescale-alist
        '((".*メイリオ.*" . 1.06)))

  ;; フレームサイズ等の設定
  (setq default-frame-alist
        (append '((left-fringe . 0)
                  (right-fringe . 0)
                  ) default-frame-alist))

  (setq default-frame-alist
        (append gui-frame-settings default-frame-alist))
  )

 ;;;;;;;;;;;;;;;;;;;; MAC OS X / GNU Emacs ;;;;;;;;;;;;;;;;;;;;
 ((and (eq system-type 'darwin)
       (eq window-system 'ns))

  ;; マシンスペックに応じて設定。処理を軽くする。
  (setq gc-cons-threshold (* gc-cons-threshold 10))

  ;; ツールバーを非表示に設定
  (tool-bar-mode -1)

  ;; スクロールバーを非表示に設定
  (scroll-bar-mode -1)

  ;; 編集中ファイルのフルパスをタイトルバーに表示する
  (setq frame-title-format
        (format "%%f  Emacs@%s" (system-name)))

  ;; 透明度の設定
  (add-to-list 'default-frame-alist
               '(alpha . 80))

  ;; 選択時の色 (文字: Black, 背景: PaleGreen)
  (set-face-foreground 'region "Black")
  (set-face-background 'region "PaleGreen")

  ;; 通常時の色 (文字: White, 背景: Black, カーソル: Gray)
  (set-foreground-color "White")
  (set-background-color "Black")
  (set-cursor-color "Gray")

  ;; デフォルトのフォント設定など (フォントセット)
  (set-face-attribute 'default nil
                      :family "Monaco"
                      :height 130)

  ;; 日本語をヒラギノ角ゴ ProN にする
  (set-fontset-font "fontset-default"
                    'japanese-jisx0208
                    '("Hiragino Maru Gothic ProN"))

  ;; 半角カナをヒラギノ角ゴ ProN にする
  (set-fontset-font "fontset-default"
                    'katakana-jisx0201
                    '("Hiragino Maru Gothic ProN"))

  ;; 日本語のフォント幅を調整して英数字二つ分と合わせる
  (add-to-list 'face-font-rescale-alist
               '(".*Hiragino Maru Gothic ProN.*" . 1.3))

  ;; フレームサイズ等の設定
  (if gui-frame-settings
      (setq initial-frame-alist gui-frame-settings)
    (set-frame-parameter nil 'fullscreen 'maximized))
  )

 ;;;;;;;;;;;;;;;;;;; macOS / Terminal ;;;;;;;;;;;;;;;;;;;;
 ((and (eq system-type 'darwin)
       (not window-system))
  )

 ;;;;;;;;;;;;;;;;;;; Linux / X ;;;;;;;;;;;;;;;;;;;;
 ((and (eq system-type 'gnu/linux)
       (eq window-system 'x))

  ;; マシンスペックに応じて設定。処理を軽くする。
  (setq gc-cons-threshold (* gc-cons-threshold 10))

  ;; メニューバーを非表示に設定
  (menu-bar-mode -1)

  ;; ツールバーを非表示に設定
  (tool-bar-mode -1)

  ;; スクロールバーを非表示に設定
  (scroll-bar-mode -1)

  ;; 編集中ファイルのフルパスをタイトルバーに表示する
  (setq frame-title-format
        (format "%%f  Emacs@%s" (system-name)))

  ;; 透明度の設定
  (add-to-list 'default-frame-alist
               '(alpha . 80))

  ;; 選択時の色 (文字: Black, 背景: PaleGreen)
  (set-face-foreground 'region "Black")
  (set-face-background 'region "PaleGreen")

  ;; 通常時の色 (文字: White, 背景: Black, カーソル: Gray)
  (set-foreground-color "White")
  (set-background-color "Black")
  (set-cursor-color "Gray")

  ;; フレームサイズ等の設定
  (if gui-frame-settings
      (setq initial-frame-alist gui-frame-settings)
    (set-frame-parameter nil 'fullscreen 'maximized))
  )

 ;;;;;;;;;;;;;;;;;;; Linux / Terminal ;;;;;;;;;;;;;;;;;;;;
 ((and (eq system-type 'gnu/linux)
       (not window-system))
  )

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (t
  (error "Unknown platform: %s, %s" system-type window-system)
  ))
