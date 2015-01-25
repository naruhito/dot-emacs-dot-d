;;; utilities.el --- Define utility functions

;; Add paths to load-path.
(defun add-to-load-path (&rest paths)
  (mapc '(lambda (path)
           (add-to-list 'load-path path))
        (mapcar 'expand-file-name paths)))

(provide 'utilities)
