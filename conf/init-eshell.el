;;; eshell.el --- the Emacs command shell
(require 'eshell)

;; 履歴など、一時ファイルの保存ディレクトリを指定
(setq eshell-directory-name (convert-standard-filename "~/.emacs.d/var/eshell/"))

(require 'em-cmpl)
(require 'em-prompt)
(require 'em-dirs)

;; 補完時に、大文字小文字を区別しない
(setq eshell-cmpl-ignore-case t)

;; 補完時に、サイクル形式ではなく別ウィンドウで候補を表示する
(setq eshell-cmpl-cycle-completions nil)

;; PAGERをcatに変更
(setenv "PAGER" "cat")

;; git ブランチ名を取得する関数
(defun git-prompt-branch-name ()
    "Get current git branch name"
    (let ((args '("symbolic-ref" "HEAD" "--short")))
      (with-temp-buffer
        (apply #'process-file "git" nil (list t nil) nil args)
        (unless (bobp)
          (goto-char (point-min))
          (buffer-substring-no-properties (point) (line-end-position))))))

;; promptに表示する文字列の変更 (git コマンドが存在する前提)
(setq eshell-prompt-function
      (lambda ()
        (let ((branch-name (git-prompt-branch-name)))
          (concat "[" (or username (getenv "USERNAME") (getenv "USER"))
                  "@" (or hostname (getenv "HOSTNAME") (getenv "USERDOMAIN"))
                  " " (eshell/pwd) " " (format "<%s>" (car default-process-coding-system))
                  (if branch-name (format " (%s)" branch-name))
                  (if (= (user-uid) 0) "]\n# " "]\n$ ")))))
(setq eshell-prompt-regexp "^[^#$]*[$#] ")

;; 変数を評価するための関数
(defun eshell/e (arg)
  (eval (read (format "%s" arg))))

;; Git for windows の linux コマンドを優先して使用するようにエイリアスを作成する関数
(defun eshell/use-gitbash (command)
  (if (eq system-type 'windows-nt)
      (eshell/alias command (concat "c:/Program\\ Files/Git/usr/bin/" command ".exe $*"))
    (message "%s" "use-gitbash is not supported.")))

;; Windows 向けの shift_jis でしか出力しないコマンドのためのデコーディング切替関数
(defun eshell/toggle-process-output-coding-system ()
  (if (eq (car default-process-coding-system) 'utf-8)
      (progn
        (setcar default-process-coding-system 'japanese-shift-jis-dos)
        (setq locale-coding-system 'japanese-shift-jis))
    (setcar default-process-coding-system 'utf-8)
    (setq locale-coding-system 'utf-8)))

(defun eshell/completion-at-point ()
  (interactive)
  (when (not (yas-expand))
    (completion-at-point)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-cmpl-mode-map (kbd "TAB") 'eshell/completion-at-point)
            (setcar default-process-coding-system 'utf-8)))

;; Mac OS Xのopenコマンド
(if (not (eq system-type 'darwin))
    (defun eshell/open (arg)
      (setq arg (encode-coding-string arg file-name-coding-system))
      (cond
       ((eq system-type 'windows-nt)
        (setq arg (replace-regexp-in-string "/" "\\\\" arg))
        (save-window-excursion
          (async-shell-command (format "start %s" arg) "*open*"))
        )
       ((eq system-type 'gnu/linux)
        (setq process-connection-type nil)
        (save-window-excursion
          (async-shell-command (format "xdg-open %s" arg) "*open*"))
        )
       (t
        (error "Unknown system type: %s" system-type)
        )) ()))

;; 現在のディレクトリでOSネイティブの外部ターミナルを開く
(defun open-native-terminal ()
  (interactive)
  ;; 現在の環境変数のコピーを作成し、このletブロック内だけで環境変数を変更する
  (let ((process-environment (copy-sequence process-environment)))
    ;; CLIツールがカラー出力を無効化しないようにダミー環境変数を解除・上書き
    (setenv "TERM" "xterm-256color")
    (setenv "INSIDE_EMACS" nil)
    (setenv "NO_COLOR" nil)
    (cond
     ((eq system-type 'windows-nt)
      (call-process "cmd.exe" nil 0 nil "/c" "start" "powershell.exe"))
     ((eq system-type 'darwin)
      (call-process "open" nil 0 nil "-a" "Terminal" "."))
     ((eq system-type 'gnu/linux)
      (call-process "gnome-terminal" nil 0 nil))
     (t
      (error "Unknown system type: %s" system-type)))))

;; Eshellエイリアス定義
(require 'em-alias)

;; 共通エイリアス
(dolist (alias '(("la" "ls -a $*")
                 ("ll" "ls -l $*")
                 ("q" "exit")
                 ("g" "git -c color.ui=always -c core.pager=cat $*")
                 ("v" "vagrant $*")
                 ("d" "docker $*")
                 ("cd" "cd $*; ls")
                 ("ff" "find-file $1 > /dev/null")))
  (eshell/alias (car alias) (cadr alias)))

;; プラットフォーム別エイリアス
(pcase system-type
  ('windows-nt
   (mapc 'eshell/use-gitbash
         '("find" "tree" "grep" "egrep" "whois" "openssl" "convert" "bash"))
   (dolist (alias '(("c" "toggle-process-output-coding-system")
                    ("diff" "c:/Program\\ Files/Git/usr/bin/diff.exe --color=always $*")
                    ("cmake" "cmake -G 'MSYS Makefiles' $*")
                    ("docker" "wslc $*")))
     (eshell/alias (car alias) (cadr alias))))

  ('darwin
   (dolist (alias '(("diff" "/usr/bin/diff $*")
                    ("grep" "/usr/bin/grep --colour=always $*")
                    ("egrep" "/usr/bin/egrep --colour=always $*")
                    ("date" "/bin/date $*")
                    ("man" "/usr/bin/man -P cat $*")
                    ("locate" "glocate $*")))
     (eshell/alias (car alias) (cadr alias))))

  ('gnu/linux
   (dolist (alias '(("diff" "/usr/bin/diff $*")
                    ("grep" "/bin/grep --colour=always $*")
                    ("egrep" "/bin/egrep --colour=always $*")
                    ("man" "/usr/bin/man -P cat $*")))
     (eshell/alias (car alias) (cadr alias)))))
