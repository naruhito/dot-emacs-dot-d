;;; init-emacs.el --- Emacs Settings

(require 'set-env-vars "~/.emacs.d/etc/set-env-vars")
(require 'auto-complete)
(require 'recentf)
(require 'cc-vars)
(require 'one-key)
(require 'key-chord)
(require 'hl-line)
(require 'server)
(require 'yasnippet-config)
(require 'view)
(require 'ruby-mode)
(require 'markdown-mode)

;; 拡張子とメジャーモードの関連づけ
(setq auto-mode-alist
      (append
       (list
        '("\\.tex$" . yatex-mode)
        '("\\.t$" . perl-mode)
        '("\\.php$" . web-mode)
        '("\\.ctp$" . web-mode)
        '("\\.erb$" . web-mode)
        '("\\.html$" . web-mode)
        '("\\.yml$" . conf-mode)
        '("file$" . ruby-mode)
        '("\\.json$" . js-mode)
        '("\\.md$" . markdown-mode)
        ) auto-mode-alist))

;; キーバインディングの設定
(defmacro defkey (keymap key command)
  `(define-key ,keymap ,(read-kbd-macro key) ,command))
(defmacro gdefkey (key command)
  `(define-key global-map ,(read-kbd-macro key) ,command))

(defkey ruby-mode-map "C-m" 'newline-and-indent)
(defkey emacs-lisp-mode-map "C-c C-d" 'lispxmp)
(defkey ac-complete-mode-map "C-n" 'ac-next)
(defkey ac-complete-mode-map "C-p" 'ac-previous)
(defkey view-mode-map "h" 'backward-char)
(defkey view-mode-map "j" 'next-line)
(defkey view-mode-map "k" 'previous-line)
(defkey view-mode-map "l" 'forward-char)
(defkey view-mode-map "J" 'View-scroll-line-forward)
(defkey view-mode-map "K" 'View-scroll-line-backward)
(defkey view-mode-map "f" 'View-scroll-page-forward)
(defkey view-mode-map "b" 'View-scroll-page-backward)
(defkey view-mode-map "C-f" 'View-scroll-page-forward)
(defkey view-mode-map "C-b" 'View-scroll-page-backward)
(defkey view-mode-map "C-d" 'View-scroll-half-page-forward)
(defkey view-mode-map "C-u" 'View-scroll-half-page-backward)
(defkey view-mode-map "?" 'View-search-regexp-backward)
(defkey view-mode-map "N" 'View-search-last-regexp-backward)
(defkey view-mode-map "G" 'View-goto-line-last)
(defkey view-mode-map "0" 'move-beginning-of-line)
(defkey view-mode-map "$" 'move-end-of-line)
(defkey view-mode-map "i" 'view-mode)
(defkey view-mode-map "q" '(lambda () (interactive)
                             (kill-buffer (current-buffer))))
(defkey view-mode-map "SPC" 'bm-toggle)
(defkey view-mode-map "[" 'bm-previous)
(defkey view-mode-map "]" 'bm-next)
(gdefkey "C-h" 'delete-backward-char)
(gdefkey "M-h" 'backward-delete-word)
(gdefkey "M-d" 'delete-word)
(gdefkey "C-m" 'newline-and-indent)
(gdefkey "C-x RET" 'switch-to-buffer)
(gdefkey "M-+" 'word-count-mode)
(gdefkey "C-M-y" 'browse-kill-ring)
(gdefkey "C-x a s" 'auto-save-buffers-toggle)
(gdefkey "C-x C-z" 'open-junk-file)
(gdefkey "C-x C-j" 'skk-mode)
(gdefkey "M-SPC" 'bm-toggle)
(gdefkey "M-[" 'bm-previous)
(gdefkey "M-]" 'bm-next)
(gdefkey "C-x y" 'yas/register-oneshot-snippet)
(gdefkey "C-x C-y" 'yas/expand-oneshot-snippet)
(gdefkey "C-c r" 'query-replace-regexp)
(gdefkey "C-c C-r" 'window-resizer)
(gdefkey "C-x C-c" 'server-edit)
(gdefkey "C-x r i" 'string-insert-rectangle)
(gdefkey "C-x C-l" 'set-buffer-file-coding-system)
(gdefkey "<f11>" 'point-undo)
(gdefkey "<f12>" 'point-redo)
(setq yas/trigger-key "I")

(add-hook 'eshell-mode-hook
          '(lambda ()
             (defkey eshell-mode-map "C-a" 'eshell-bol)
             (defkey eshell-mode-map "C-M-p" 'eshell-previous-matching-input-from-input)
             (defkey eshell-mode-map "C-M-n" 'eshell-next-matching-input-from-input)
             ))

(defvar one-key-menu-alist-global nil)
(setq one-key-menu-alist-global
      '((("C-a" . "Apropos") . apropos)
        (("C-b" . "Browse Structure") . anything-imenu)
        (("C-c" . "Calendar") . calendar)
        (("C-d" . "Define a New Snippet") . yas/new-snippet)
        (("C-e" . "Execute grep-find") . moccur-grep-find)
        (("C-f" . "Clock out") . timeclock-out)
        (("C-h" . "Clock in") . timeclock-in)
        (("C-i" . "Eshell") . eshell)
        (("C-j" . "Jump to Documents") . anything-apropos)
        (("C-k" . "Google Search") . web-search-query-default)
        (("C-l" . "Look for on the Web") . web-search-query)
        (("C-m" . "Moccur") . moccur)
        (("C-n" . "Next Change") . goto-last-change)
        (("C-o" . "Open File/Buffer") . anything)
        (("C-p" . "Previous Change") . goto-last-change-reverse)
        (("C-r" . "Regexp Builder") . re-builder)
        (("C-s" . "Save the Last Keyboard Macro") . kmacro-save)
        (("C-t" . "Describe Mode") . describe-mode)
        (("C-u" . "Look up on my Gist") . gist-list)
        (("C-v" . "Value Search") . apropos-value)
        (("C-w" . "What is this Key?") . describe-key)
        (("C-x" . "Revert Buffer with utf-8-unix") . revert-buffer-with-coding-system-utf-8-unix)
        (("C-y" . "Yank Buffer to Gist") . gist-buffer)
        (("C-z" . "Suspend Frame") . suspend-frame)
        (("c" . "Customize Face") . list-faces-display)
        (("f" . "Face at Point") . describe-face-at-point)
        (("i" . "Check Spell Region") . ispell)
        (("j" . "Jump to URL at Point") . browse-url-at-point)
        (("k" . "Web Search Region") . web-search-region)
        (("l" . "Call Last kbd macro") . kmacro-end-or-call-macro)
        (("n" . "Create New Junk File") . create-new-junk-file)
        (("o" . "Reopen Junk File at Point") . re-open-junk-file-at-point)
        (("p" . "Web Search at Point") . web-search-at-point)
        (("q" . "Quit") . save-buffers-kill-emacs)
        (("w" . "Copy Current Junk Filename") . copy-current-junk-filename)
        ))

(defun one-key-menu-global ()
  (interactive)
  (one-key-menu "Global Menu" one-key-menu-alist-global))

(gdefkey "C-z" 'one-key-menu-global)

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
                    )
                   (t                   ;Windows以外の環境はすべて utf-8 で統一
                    (setq default-process-coding-system '(utf-8 . utf-8))
                    (setq default-file-name-coding-system 'utf-8)
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

;; ファイルを開いたときなどに "utf-8-unix" でなければ変換するか尋ねる
(defun set-buffer-file-coding-system-utf-8-unix ()
  (if (and (not (coding-system-equal buffer-file-coding-system 'utf-8-unix))
           (not (coding-system-equal buffer-file-coding-system 'undecided-unix))
           (y-or-n-p "Set buffer-file-coding-system as utf-8-unix? "))
      (set-buffer-file-coding-system 'utf-8-unix)))
(add-hook 'find-file-hooks 'set-buffer-file-coding-system-utf-8-unix)
(add-hook 'after-revert-hook 'set-buffer-file-coding-system-utf-8-unix)

;; エディタが誤認識した場合等に使用。"utf-8-unix"で開きなおす。
(defun revert-buffer-with-coding-system-utf-8-unix ()
  (interactive)
  (if (not (coding-system-equal buffer-file-coding-system 'utf-8-unix))
      (revert-buffer-with-coding-system 'utf-8-unix)))

;; 起動時のカレントディレクトリをホームディレクトリに設定
(cd "~")

;; Emacs終了時にバイトコンパイルを実行 (GitPullによる、".el"と".elc"の不整合問題への対策)
(add-hook 'kill-emacs-hook '(lambda ()
                              (byte-recompile-directory "~/.emacs.d" 0)))

;; エラーが起きてもデバッガが立ち上がらないようにする
(setq debug-on-error nil)

;; recentfの保存場所を変更
(setq recentf-save-file (convert-standard-filename "~/.emacs.d/var/recentf"))

;; BackUpFileを作らない
(setq make-backup-files nil)

;; 自動保存用ファイル (#*#) も作らない
(setq auto-save-default nil)
(setq auto-save-list-file-name "~/.emacs.d/var/auto-save-list")
(setq auto-save-list-file-prefix nil)

;; 静的略語定義を保存しない
(setq save-abbrevs nil)

;; インデントではモードを問わず空白を挿入する
(setq-default tab-width 4 indent-tabs-mode nil)
(setq c-basic-offset 4)

;; upcase-word の対象を直前の単語に変更
(defadvice upcase-word (before before-upcase-word)
  (backward-word))
(ad-activate 'upcase-word)

;; ファイル名やバッファ名の補完で大文字小文字を区別しない
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

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

;; emacs-serverを (再) 起動する
(if (server-running-p)
    (server-force-delete))
(server-start)

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
                      :family "ＭＳ ゴシック"
                      :height 140)

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

 ;;;;;;;;;;;;;;;;;;;; MAC OS X / Cocoa Emacs ;;;;;;;;;;;;;;;;;;;;
 ((and (eq system-type 'darwin)
       (eq window-system 'ns))

  ;; 画面横に出るスクロールバーを消す
  (scroll-bar-mode -1)

  ;; 透明度の設定
  (add-to-list 'default-frame-alist
               '(alpha . 75))

  ;; 選択時の色 (文字: Black, 背景: PaleGreen)
  (set-face-foreground 'region "Black")
  (set-face-background 'region "PaleGreen")

  ;; 通常時の色 (文字: White, 背景: Black, カーソル: Gray)
  (set-foreground-color "White")
  (set-background-color "Black")
  (set-cursor-color "Gray")

  ;; デフォルトのフォント設定など (フォントセット)
  (set-face-attribute 'default nil
                      :family "monaco"
                      :height 140)

  ;; フォントセットのうち、日本語のフォントだけ変更
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    '("Hiragino Maru Gothic Pro" . "iso10646-1"))

  (set-fontset-font (frame-parameter nil 'font)
                    'katakana-jisx0201
                    '("Hiragino Maru Gothic Pro" . "iso10646-1"))

  ;; 特定のフォントについて、文字幅を調整
  (setq face-font-rescale-alist
        '(("^-apple-hiragino.*" . 1.0)
          (".*osaka-bold.*" . 1.0)
          (".*osaka-medium.*" . 1.0)
          (".*courier-bold-.*-mac-roman" . 0.8)
          (".*monaco cy-bold-.*-mac-cyrillic" . 0.8)
          (".*monaco-bold-.*-mac-roman" . 0.8)
          ("-cdac$" . 0.9)))

  ;; フレームサイズ等の設定
  (setq initial-frame-alist gui-frame-settings)
  )

 ;;;;;;;;;;;;;;;;;;; MAC OS X / Terminal ;;;;;;;;;;;;;;;;;;;;
 ((and (eq system-type 'darwin)
       (not window-system))
  )

 ;;;;;;;;;;;;;;;;;;; Linux / X ;;;;;;;;;;;;;;;;;;;;
 ((and (eq system-type 'gnu/linux)
       (eq window-system 'x))
  )

 ;;;;;;;;;;;;;;;;;;; Linux / Terminal ;;;;;;;;;;;;;;;;;;;;
 ((and (eq system-type 'gnu/linux)
       (not window-system))
  )

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (t
  (error "Unknown platform: %s, %s" system-type window-system)
  ))
