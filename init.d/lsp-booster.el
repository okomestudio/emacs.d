;;; lsp-booster.el --- lsp-booster  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; `lsp-booster' minor mode. See github.com/blahgeek/emacs-lsp-booster.
;;
;;; Code:

(defvar lsp-booster-mode nil
  "Set non-nil when `lsp-booster-mode' is active, nil otherwise.")

(defun lsp-booster--advice-json-parse (fn &rest args)
  "Advise (FN ARGS) to parse bytecode instead of JSON."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply fn args)))

(defun lsp-booster--advice-final-command (fn cmd &optional test?)
  "Advise FN to prepend `emacs-lsp-booster' to CMD if TEST? is nil."
  (let ((orig-result (funcall fn cmd test?)))
    (if (and (not test?) ;; for check lsp-server-present?
             ;; see lsp-resolve-final-command, it would add extra shell wrapper
             (not (file-remote-p default-directory))
             lsp-use-plists
             (not (functionp 'json-rpc-connection)) ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(defun lsp-booster--workspace-shutdown-all ()
  "Shut down all running LSP servers.
The function returns LSP servers that have been shut down."
  (let ((workspaces (lsp-workspaces)))
    (dolist (workspace workspaces)
      (lsp-workspace-shutdown workspace))
    workspaces))

;;;###autoload
(defun lsp-booster-mode (&optional arg)
  "Mode for lsp-booster.
If called from Lisp, toggle the mode if ARG is nil. Enable the
mode if ARG is a positive number. Disable the mode if ARG is zero
or a negative number."
  (interactive "P")
  (setq lsp-booster-mode (if (null arg)
                             (not lsp-booster-mode)
                           (> (prefix-numeric-value arg) 0)))

  (setq lsp-booster--json-reader
        (if (progn (require 'json)
                   (fboundp 'json-parse-buffer))
            'json-parse-buffer
          'json-read))

  (let ((workspaces (and (featurep 'lsp-mode)
                         (lsp-booster--workspace-shutdown-all))))
    (if lsp-booster-mode
        (progn
          (message "lsp-booster-mode on")
          (advice-add lsp-booster--json-reader :around
                      #'lsp-booster--advice-json-parse)
          (advice-add 'lsp-resolve-final-command :around
                      #'lsp-booster--advice-final-command)
          (when workspaces
            (lsp-deferred)))
      (message "lsp-booster-mode off")
      (advice-remove 'lsp-resolve-final-command
                     #'lsp-booster--advice-final-command)
      (advice-remove lsp-booster--json-reader
                     #'lsp-booster--advice-json-parse)
      (when workspaces
        (lsp-deferred)))))

(provide 'lsp-booster)
;;; lsp-booster.el ends here
