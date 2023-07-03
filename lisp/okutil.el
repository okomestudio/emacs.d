;;; okutil.el --- Okome Studio utilities  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Okome Studio utilities.
;;
;;; Code:


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
