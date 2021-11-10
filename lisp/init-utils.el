;;; init-utils.el --- Utility functions  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun debug-message (text)
  "Print a debug message TEXT with timestamp."
  (message "%s: %s" (format-time-string "%FT%H:%M:%S.%3N" (current-time)) text))


(defun ensure-directory-exists (path)
  "Ensure the directory exists at PATH."
  (if (not (file-directory-p path))
      (make-directory path :parents)))


(defun ensure-file-from-url (src &optional dest)
  "Ensure that file at URL gets downloaded and exists.

SRC is the source URL, DEST is the local destination path for the
downloaded file. If DEST is not given, the filename is inferred
from the source path. If DEST is not an absolute path, the file
will be created in the my-lipdir directory."
  (let ((dest (if dest
                  (if (string-prefix-p "/" dest)
                      dest
                    (concat (file-name-as-directory ts/site-lisp-dir) dest))
                (concat (file-name-as-directory ts/site-lisp-dir)
                        (file-name-nondirectory src)))))
    (if (not (file-exists-p dest))
        (url-copy-file src dest))))


(defun ensure-file-from-github (src &optional dest)
  "Ensure that a file hosted by GitHub gets downloaded and exists.

SRC is the source path in GitHub, DEST is the local destination
path for the downloaded file. See ensure-file-from-url for
detail."
  (ensure-file-from-url
   (concat "https://raw.githubusercontent.com/" src) dest))


(defun parent-directory (path)
  "Get the parent directory of PATH.

This function effectively removes the last component from PATH,
regardless of it being a file or a directory. The returned parent
directory always ends with a '/' character. The function returns
nil if no parent directory exists (i.e., PATH points to root)."
  (unless (equal "/" path)
    (file-name-directory (directory-file-name path))))


(defun remove-trailing-whitespaces-on-save ()
  "Remove trailing whitespaces on save.

  Use this function with a mode hook."
  (add-hook 'local-write-file-hooks
            '(lambda () (save-excursion (delete-trailing-whitespace)))))


(defun sort-lines-ci ()
  "Sort lines case-insensitively."
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))


(defun ts/font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))


(defun ts/locate-dominating-files (file name)
  "Look upward in directory hierarchy from FILE to locate ones containing NAME.

This is an extended version `locate-dominating-file`, which does
not stop at the first occurrence of NAME and continues looking
upward."
  (let* ((dir-locals-file (locate-dominating-file file name))
         (dir-locals-files '()))
    (while dir-locals-file
      (progn
        (add-to-list 'dir-locals-files dir-locals-file)
        (setq dir-locals-file (locate-dominating-file
                               (parent-directory dir-locals-file) name))))
    dir-locals-files))

(defun ts/reload-dir-locals-for-current-buffer ()
  "Reload dir locals for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables)))

(provide 'init-utils)
;;; init-utils.el ends here
