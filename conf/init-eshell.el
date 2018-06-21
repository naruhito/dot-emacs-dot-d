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

;; 環境変数PATHを追加
(let (separator additional-paths)
  (cond
   ((eq system-type 'windows-nt)
    (setq separator ";")
    (setq additional-paths (list))
    )
   ((eq system-type 'darwin)
    (setq separator ":")
    (setq additional-paths (list "/usr/local/bin"))
    )
   ((eq system-type 'gnu/linux)
    (setq separator ":")
    (setq additional-paths (list))
    )
   (t
    (error "Unknown system type: %s" system-type)
    ))
  (setq additional-paths
        (mapcar 'expand-file-name additional-paths))
  (setenv "PATH" (concat
                  (getenv "PATH") separator
                  (mapconcat 'identity additional-paths separator)
                  ))
  (setq eshell-path-env (getenv "PATH"))
  (setq exec-path (append exec-path additional-paths))
  )

;; promptに表示する文字列の変更
(setq eshell-prompt-function
      (lambda ()
        (concat "[" (or username (getenv "USERNAME") (getenv "USER"))
                "@" (or hostname (getenv "HOSTNAME") (getenv "USERDOMAIN"))
                " " (eshell/pwd) " " (format "<%s>" (car default-process-coding-system))
                (if (= (user-uid) 0) "]\n# " "]\n$ "))))
(setq eshell-prompt-regexp "^[^#$]*[$#] ")

;; 変数を評価するための関数
(defun eshell/e (arg)
  (eval (read (format "%s" arg))))

;; Cygwin コマンドを優先して使用するようにエイリアスを作成する関数
(defun eshell/use-cygwin (command)
  (if (eq system-type 'windows-nt)
      (eshell/alias command (concat "c:/cygwin/bin/" command ".exe $*"))
    (message "%s" "use-cygwin is not supported.")))

;; Windows 向けの shift_jis でしか出力しないコマンドのためのデコーディング切替関数
(defun eshell/toggle-process-output-coding-system ()
  (cond
   ((eq (car default-process-coding-system) 'utf-8)
    (setcar default-process-coding-system 'japanese-shift-jis-dos)
    )
   (t
    (setcar default-process-coding-system 'utf-8)
    )))
(add-hook 'eshell-mode-hook '(lambda () (setcar default-process-coding-system 'utf-8))) ;default

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
(eshell/alias "g" "git $*")
(eshell/alias "v" "vagrant $*")
(eshell/alias "d" "docker $*")
(eshell/alias "cd" "cd $*; ls")
(eshell/alias "ff" "find-file $1 > /dev/null")
(cond
 ((eq system-type 'windows-nt)
  (mapcar 'eshell/use-cygwin
          (list "find" "tree" "grep" "egrep" "whois" "openssl" "diff" "convert"))
  (eshell/alias "c" "toggle-process-output-coding-system")
  (eshell/alias "ll" "c:/cygwin/bin/ls.exe -l $*")  ;"eshell/ls"はパーミッションの表示がおかしい
  )
 ((eq system-type 'darwin)
  (eshell/alias "diff" "/usr/bin/diff $*")
  (eshell/alias "grep" "/usr/bin/grep $*")
  (eshell/alias "egrep" "/usr/bin/egrep $*")
  (eshell/alias "date" "/bin/date $*")
  )
 ((eq system-type 'gnu/linux)
  (eshell/alias "diff" "/usr/bin/diff $*")
  (eshell/alias "grep" "/bin/grep $*")
  ))
