;;; okutil.el --- Okome Studio utilities  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Okome Studio utilities.
;;
;;; Code:


(defun okutil-fill-or-unfill-paragraph (&optional region)
  "Fill or unfill a paragraph in REGION."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (if (> (- (line-end-position) (line-beginning-position)) fill-column)
      (fill-paragraph nil region)
    (let* ((fill-column (point-max))
           (emacs-lisp-docstring-fill-column t))
      (fill-paragraph nil region))))


(defun okutil-org-fill-or-unfill-paragraph (&optional region)
  "Fill or unfill an Org paragraph in REGION."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (require 'org)
  (if (> (- (line-end-position) (line-beginning-position)) fill-column)
      (org-fill-paragraph nil region)
    (let* ((fill-column (point-max))
           (emacs-lisp-docstring-fill-column t))
      (org-fill-paragraph nil region))))


(defun okutil-string-from-region-or-prompt (prompt &optional initial history default inherit)
  "Read string from region when active; otherwise, get it from PROMPT.

See `read-string` for the meaning of INITIAL, HISTORY, DEFAULT, and INHERIT."
  (if (region-active-p)
      (prog1
          (buffer-substring-no-properties (region-beginning) (region-end))
        (deactivate-mark)
        (message ""))
    (read-string prompt initial history default inherit)))


(provide 'okutil)
;;; okutil.el ends here
