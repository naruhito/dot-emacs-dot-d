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
        (concat "[" (getenv "USERNAME") "@" (getenv "HOSTNAME")  " "
                (eshell/pwd)
                (if (= (user-uid) 0) "]\n# " "]\n$ "))))
(setq eshell-prompt-regexp "^[^#$]*[$#] ")

;; Eshellエイリアス定義
(require 'em-alias)
(eshell/alias "la" "ls -a $*")
(eshell/alias "ll" "ls -l $*")
(eshell/alias "q" "exit")
(eshell/alias "g" "git $*")
(eshell/alias "s" "screen $*")
(eshell/alias "v" "vagrant $*")
(eshell/alias "p" "perl $*")
(eshell/alias "cd" "cd $*; ls")
(eshell/alias "ff" "find-file $1 > /dev/null")
(cond
 ((eq system-type 'windows-nt)
  (eshell/alias "find" "c:/cygwin/bin/find.exe $*")
  (eshell/alias "tree" "c:/cygwin/bin/tree.exe $*")
  (eshell/alias "grep" "c:/cygwin/bin/grep.exe $*")
  (eshell/alias "egrep" "c:/cygwin/bin/egrep.exe $*")
  (eshell/alias "whois" "c:/cygwin/bin/whois.exe $*")
  (eshell/alias "openssl" "c:/cygwin/bin/openssl.exe $*")
  (eshell/alias "ll" "c:/cygwin/bin/ls.exe -l $*")        ;"eshell/ls"はパーミッションの表示がおかしい
  (eshell/alias "diff" "c:/cygwin/bin/diff.exe -us $*")
  (eshell/alias "convert" "c:/cygwin/bin/convert.exe $*")
  )
 ((eq system-type 'darwin)
  (eshell/alias "diff" "/usr/bin/diff -us $*")
  )
 ((eq system-type 'gnu/linux)
  (eshell/alias "diff" "/usr/bin/diff -us $*")
  ))

;; 変数を評価するための関数
(defun eshell/e (arg)
  (eval (read (format "%s" arg))))

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
        (message "%s" "open is not supported.")
        )
       (t
        (error "Unknown system type: %s" system-type)
        )) ()))
