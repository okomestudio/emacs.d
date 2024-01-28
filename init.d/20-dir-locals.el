;;; 20-dir-locals.el --- Utilities for dir locals  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defgroup init-dir-locals nil
  "Customization group for init-dir-locals.")


;; Add an advice so that .dir-locals.el are looked for upward in the directory hierarchy.

(defcustom init-dir-locals-walk-dir-locals-upward t
  "If t, look for .dir-locals.el up in directory hierarchy."
  :type '(bool)
  :group 'init-dir-locals)


(defun init-dir-locals--hack-dir-local-variables-advice (func)
  "If deir-locals-utils-walk-dir-locals-upward is non-nil, evaluate
.dir-locals.el files starting in the current directory and going
up. Otherwise they will be evaluated from the top down to the
current directory."
  (when init-dir-locals-walk-dir-locals-upward
    (require 'okutil)
    (let ((dir-locals-file ".dir-locals.el")
          (original-buffer-file-name (buffer-file-name))
          (nesting (okutil-locate-dominating-files
                    (or (buffer-file-name) default-directory) dir-locals-file)))
      (unwind-protect
          (dolist (name nesting)
            ;; make it look like a file higher up in the hierarchy is visited
            (setq buffer-file-name (concat name dir-locals-file))
            (funcall func))
        (setq buffer-file-name original-buffer-file-name)))
    (funcall func)))


(advice-add 'hack-dir-local-variables
            :around #'init-dir-locals--hack-dir-local-variables-advice)


;; Reloading utilities
;; -------------------
;;
;; See https://emacs.stackexchange.com/a/13096/599.


(defun init-dir-locals-reload-for-current-buffer ()
  "Reload dir-locals for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))


(defvar default-directory-symlinked nil
  "Set default-directory for symlinked .dir-locals.el.

Set this within symlinked .dir-locals.el.")

(put 'default-directory-symlinked 'safe-local-variable #'stringp)


(defun init-dir-locals-reload-for-all-buffers ()
  "Reload dir-locals for all buffers in `default-directory`."
  (interactive)
  (let ((dir (or default-directory-symlinked
                 default-directory)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (init-dir-locals-reload-for-current-buffer))))))


(add-hook 'emacs-lisp-mode-hook
          (defun enable-autoreload-for-dir-locals ()
            (when (and (buffer-file-name)
                       (equal dir-locals-file
                              (file-name-nondirectory (buffer-file-name))))
              (add-hook 'after-save-hook
                        'init-dir-locals-reload-for-all-buffers
                        nil t))))


(add-hook 'lisp-data-mode-hook
          (lambda ()
            (interactive)
            (when (and (buffer-file-name)
                       (equal dir-locals-file
                              (file-name-nondirectory (buffer-file-name))))
              (add-hook 'after-save-hook
                        'init-dir-locals-reload-for-all-buffers
                        nil t))))

;;; 20-dir-locals.el ends here
