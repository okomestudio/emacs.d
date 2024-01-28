;;; okutil.el --- Okome Studio utilities  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Okome Studio utilities.
;;
;;; Code:


(defun okutil-color-scale (color factor)
  "Scale COLOR lighter or darker by a numeric FACTOR.

FACTOR is between 0.0 to 2.0, where 1.0 means unchanged.

Example:

  (okutil-color-scale '(#x55 #x44 #x33) 0.0)  ;; black
  (okutil-color-scale '(#x55 #x44 #x33) 0.8)  ;; darker
  (okutil-color-scale '(#x55 #x44 #x33) 1.0)  ;; unchanged
  (okutil-color-scale '(#x55 #x44 #x33) 1.2)  ;; lighter
  (okutil-color-scale '(#x55 #x44 #x33) 2.0)  ;; white"
  (let* ((result 0.0))
    (cl-loop
     for pow downfrom 2
     for x in color
     do
     (setq result
           (+ result
              (* (expt 256 pow)
                 (min 255
                      (max 0
                           (round
                            (if (< factor 1.0)
                                (* x factor)
                              (+ x (* (- 255 x) (- factor 1.0)))))))))))
    (format "#%X" result)))


(defun okutil-debug-message (text)
  "Print a debug message TEXT with timestamp."
  (message "%s: %s"
           (format-time-string "%FT%H:%M:%S.%3N" (current-time))
           text))


(defun okutil-ensure-directory-exists (path)
  "Ensure the directory exists at PATH."
  (if (not (file-directory-p path))
      (make-directory path :parents)))


(defun okutil-ensure-file-from-github (src &optional dest)
  "Ensure that a file hosted by GitHub gets downloaded and exists.

SRC is the source path in GitHub, DEST is the local destination
path for the downloaded file. See okutil-ensure-file-from-url for
detail."
  (okutil-ensure-file-from-url
   (concat "https://raw.githubusercontent.com/" src) dest))


(defun okutil-ensure-file-from-url (src &optional dest)
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


(defun okutil-fill-or-unfill-paragraph (&optional region)
  "Fill or unfill a paragraph in REGION."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (if (> (- (line-end-position) (line-beginning-position)) fill-column)
      (fill-paragraph nil region)
    (let* ((fill-column (point-max))
           (emacs-lisp-docstring-fill-column t))
      (fill-paragraph nil region))))


(defun okutil-font-installed-p (font-name)
  "Check if font named FONT-NAME is installed."
  ;; Another function, x-list-fonts, could also be used.
  (find-font (font-spec :name font-name)))


(defun okutil-hatena-visit-bookmark-comments (arg &optional url)
  "Visit the Hatena Bookmark comments page for the URL.

If URL is not supplied, the function will attempt to yank one
from kill ring. The page will be opened with eww or with an
external browser if prefixed."
  (interactive "P")
  (let* ((url (if (equal url nil) (current-kill 0) url))
         (hatena-url (string-replace "https://" "https://b.hatena.ne.jp/entry/s/" url)))
    (okutil-url-visit arg hatena-url)))


(defun okutil-insert-newline-above ()
  "Insert a new line above current point."
  (interactive)
  (back-to-indentation)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))


(defun okutil-insert-newline-below ()
  "Insert a new line below current point."
  (interactive)
  (end-of-line)
  (newline-and-indent))


(defun okutil-insert-zero-width-space ()
  "Insert a zero width space character at point."
  (interactive)
  (insert-char #x200b))


(defun okutil-is-linux-p ()
  "Return non-nil if system is Linux-based."
  (string-equal system-type "gnu/linux"))


(defun okutil-is-macos-p ()
  "Return non-nil if system is Mac OS X-based."
  (string-equal system-type "darwin"))


(defun okutil-locate-dominating-files (file name)
  "Look upward from FIL in directory hierarchy to locate files named NAME.

This extends the `locate-dominating-file` function not to stop at
the first occurrence of NAME and continues looking upward in the
directory tree."
  (let* ((dir-locals-file (locate-dominating-file file name))
         (dir-locals-files '())
         (parent-dir nil))
    (while dir-locals-file
      (push dir-locals-file dir-locals-files)
      (setq parent-dir (okutil-parent-directory dir-locals-file))
      (setq dir-locals-file
            (cond ((not (eq parent-dir nil)) (locate-dominating-file parent-dir name))
                  (t nil))))
    dir-locals-files))


(defun okutil-monitor-count ()
  "Get the number of display monitors."
  (length (display-monitor-attributes-list)))


(defun okutil-org-fill-or-unfill-paragraph (&optional region)
  "Fill or unfill an Org paragraph in REGION."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (require 'org)
  (if (> (- (line-end-position) (line-beginning-position)) fill-column)
      (org-fill-paragraph nil region)
    (let* ((fill-column (point-max))
           (emacs-lisp-docstring-fill-column t))
      (org-fill-paragraph nil region))))


(defun okutil-parent-directory (path)
  "Get the parent directory of PATH.

This function effectively removes the last component from PATH,
regardless of the path pointing to a file or a directory. The
returned directory always ends with a '/' character. The function
returns nil if no parent directory exists (i.e., PATH points to
root)."
  (unless (equal "/" path)
    (file-name-directory (directory-file-name (expand-file-name path)))))


(defun okutil-revert-buffer-no-confirm (&optional force-reverting)
  "Interactive call to 'revert-buffer'.

Ignoring the auto-save file and not requesting for confirmation.
When the current buffer is modified, the command refuses to
revert it, unless you specify the optional argument:
FORCE-REVERTING to true."
  (interactive "P")
  (if (or force-reverting (not (buffer-modified-p)))
      (revert-buffer :ignore-auto :noconfirm)
    (error "The buffer has been modified")))


(defun okutil-sort-lines-ci ()
  "Sort lines case-insensitively."
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))


(defun okutil-string-from-region-or-prompt (prompt &optional initial history default inherit)
  "Read string from region when active; otherwise, get it from PROMPT.

See `read-string` for the meaning of INITIAL, HISTORY, DEFAULT, and INHERIT."
  (if (region-active-p)
      (prog1
          (buffer-substring-no-properties (region-beginning) (region-end))
        (deactivate-mark)
        (message ""))
    (read-string prompt initial history default inherit)))


(defun okutil-url-visit (arg url)
  "Visit URL with eww or an external browser if prefixed."
  (interactive "P")
  (pcase arg
    ('(4) (browse-url url))
    (_ (eww url))))


(provide 'okutil)
;;; okutil.el ends here
