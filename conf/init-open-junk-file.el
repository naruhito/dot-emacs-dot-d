;;; open-junk-file.el --- Open a junk (memo) file to try-and-error
(require 'open-junk-file)
(setq open-junk-file-format "~/.emacs.d/var/notes/%Y/%m/%d-%H%M%S.")

(defun create-new-junk-file ()
  (interactive)
  (let* ((file (format-time-string open-junk-file-format (current-time)))
         (dir (file-name-directory file))
         (filename (concat file "org")))
    (make-directory dir t)
    (find-file-noselect filename)
    (insert (substring filename -21))))

(defun re-open-junk-file-at-point ()
  (interactive)
  (let ((s (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (when (string-match (rx (= 4 (any "0-9")) "/" (= 2 (any "0-9")) "/" (= 2 (any "0-9")) "-" (= 6 (any "0-9")) ".org") s)
      (find-file (concat "~/.emacs.d/var/notes/" (match-string 0 s))))))

(defun copy-current-junk-filename ()
  (interactive)
  (kill-new
   (substring
    (concat (file-name-as-directory (expand-file-name "."))
            (buffer-name (current-buffer))) -21)))
