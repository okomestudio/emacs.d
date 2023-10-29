;;; dir-locals-utils.el --- Utilities for dir locals  -*- lexical-binding: t -*-
;;; Commentary:
;;
;;
;;; Code:

(require 'init-utils)


(defgroup dir-locals-utils nil
  "Customization group for dir-locals-utils.")


;; Add an advice so that .dir-locals.el are looked for upward in the directory hierarchy.

(defcustom dir-locals-utils-walk-dir-locals-upward t
  "If t, look for .dir-locals.el up in directory hierarchy."
  :type '(bool)
  :group 'dir-locals-utils)


(defun dir-locals-utils--hack-dir-local-variables-advice (func)
  "If deir-locals-utils-walk-dir-locals-upward is non-nil, evaluate
.dir-locals.el files starting in the current directory and going
up. Otherwise they will be evaluated from the top down to the
current directory."
  (when dir-locals-utils-walk-dir-locals-upward
    (let ((dir-locals-file ".dir-locals.el")
          (original-buffer-file-name (buffer-file-name))
          (nesting (ts/locate-dominating-files
                    (or (buffer-file-name) default-directory) dir-locals-file)))
      (unwind-protect
          (dolist (name nesting)
            ;; make it look like a file higher up in the hierarchy is visited
            (setq buffer-file-name (concat name dir-locals-file))
            (funcall func))
        (setq buffer-file-name original-buffer-file-name)))
    (funcall func)))


(advice-add 'hack-dir-local-variables
            :around #'dir-locals-utils--hack-dir-local-variables-advice)


;; Reloading utilities
;; -------------------
;;
;; See https://emacs.stackexchange.com/a/13096/599.


(defun dir-locals-utils-reload-for-current-buffer ()
  "Reload dir-locals for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))


(defvar default-directory-symlinked nil
  "Set default-directory for symlinked .dir-locals.el.

Set this within symlinked .dir-locals.el.")

(put 'default-directory-symlinked 'safe-local-variable #'stringp)


(defun dir-locals-utils-reload-for-all-buffers ()
  "Reload dir-locals for all buffers in `default-directory`."
  (interactive)
  (let ((dir (or default-directory-symlinked
                 default-directory)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (dir-locals-utils-reload-for-current-buffer))))))


(add-hook 'emacs-lisp-mode-hook
          (defun enable-autoreload-for-dir-locals ()
            (when (and (buffer-file-name)
                       (equal dir-locals-file
                              (file-name-nondirectory (buffer-file-name))))
              (add-hook 'after-save-hook
                        'dir-locals-utils-reload-for-all-buffers
                        nil t))))


(add-hook 'lisp-data-mode-hook
          (lambda ()
            (interactive)
            (when (and (buffer-file-name)
                       (equal dir-locals-file
                              (file-name-nondirectory (buffer-file-name))))
              (add-hook 'after-save-hook
                        'dir-locals-utils-reload-for-all-buffers
                        nil t))))


(provide 'dir-locals-utils)
;;; dir-locals-utils.el ends here
