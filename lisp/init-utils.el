;;; init-utils.el --- Utility functions  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


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


(provide 'init-utils)
;;; init-utils.el ends here
