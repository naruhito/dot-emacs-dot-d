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
  (cond
   ((eq (car default-process-coding-system) 'utf-8)
    (setcar default-process-coding-system 'japanese-shift-jis-dos)
    (setq locale-coding-system 'japanese-shift-jis)
    )
   (t
    (setcar default-process-coding-system 'utf-8)
    (setq locale-coding-system 'utf-8)
    )))

(defun eshell/completion-at-point ()
  (interactive)
  (when (not (yas-expand))
    (completion-at-point)))

(add-hook 'eshell-mode-hook '(lambda ()
                               (defkey eshell-cmpl-mode-map "TAB" 'eshell/completion-at-point)
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

;; Eshellエイリアス定義
(require 'em-alias)
(eshell/alias "la" "ls -a $*")
(eshell/alias "ll" "ls -l $*")
(eshell/alias "q" "exit")
(eshell/alias "g" "git -c color.ui=always -c core.pager=cat $*")
(eshell/alias "v" "vagrant $*")
(eshell/alias "d" "docker $*")
(eshell/alias "cd" "cd $*; ls")
(eshell/alias "ff" "find-file $1 > /dev/null")
(cond
 ((eq system-type 'windows-nt)
  (mapcar 'eshell/use-gitbash
          (list "find" "tree" "grep" "egrep" "whois" "openssl" "convert" "bash"))
  (eshell/alias "c" "toggle-process-output-coding-system")
  (eshell/alias "diff" "c:/Program\\ Files/Git/usr/bin/diff.exe --color=always $*")
  (eshell/alias "cmake" "cmake -G 'MSYS Makefiles' $*")
  )
 ((eq system-type 'darwin)
  (eshell/alias "diff" "/usr/bin/diff $*")
  (eshell/alias "grep" "/usr/bin/grep --colour=always $*")
  (eshell/alias "egrep" "/usr/bin/egrep --colour=always $*")
  (eshell/alias "date" "/bin/date $*")
  (eshell/alias "man" "/usr/bin/man -P cat $*")
  (eshell/alias "locate" "glocate $*")
  )
 ((eq system-type 'gnu/linux)
  (eshell/alias "diff" "/usr/bin/diff $*")
  (eshell/alias "grep" "/bin/grep --colour=always $*")
  (eshell/alias "grep" "/bin/egrep --colour=always $*")
  (eshell/alias "man" "/usr/bin/man -P cat $*")
  ))
