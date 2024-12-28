;;; subsys-dir-locals.el --- .dir-locals.el Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the .dir-locals.el subsystem.
;;
;;; Code:

(defvar default-directory-symlinked nil
  "Set `default-directory' for symlinked .dir-locals.el.

Set this within symlinked .dir-locals.el.")

(defvar ok-dir-locals-walk-upward t
  "If non-nil, look for .dir-locals.el up in the directory tree.")

(put 'default-directory-symlinked 'safe-local-variable #'stringp)

;; Patch `hack-dir-local-variables' so that all .dir-locals.el files
;; are looked for upward in the directory tree.
(defun ok-dir-locals--hack-dir-local-variables (orig-fun)
  "Advice ORIG-FUN, which is `hack-dir-local-variables'."
  (when ok-dir-locals-walk-upward
    (let* ((dir-locals-file ".dir-locals.el")
           (orig-buffer-file-name (buffer-file-name))
           (nesting (ok-file-locate-dominating-files (or orig-buffer-file-name
                                                         default-directory)
                                                     dir-locals-file)))
      (unwind-protect
          (dolist (name nesting)
            ;; make it look like a file higher up in the hierarchy is visited
            (setq buffer-file-name (concat name dir-locals-file))
            (funcall orig-fun))
        (setq buffer-file-name orig-buffer-file-name)))
    (funcall orig-fun)))

(advice-add 'hack-dir-local-variables :around #'ok-dir-locals--hack-dir-local-variables)


;; .dir-locals.el reloading utilities
;;
;; See https://emacs.stackexchange.com/a/13096/599.

(defun ok-dir-locals-reload-for-current-buffer ()
  "Reload currently visited .dir-locals.el buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun ok-dir-locals-reload-for-all-buffers ()
  "Reload .dir-locals.el for all buffers in `default-directory'."
  (interactive)
  (let ((dir (or default-directory-symlinked default-directory)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (ok-dir-locals-reload-for-current-buffer))))))

(defun ok-dir-locals--add-reload-hook ()
  "Add `ok-dir-locals-reload-for-all-buffers' hook."
  (when (and (buffer-file-name)
             (equal dir-locals-file
                    (file-name-nondirectory (buffer-file-name))))
    (add-hook 'after-save-hook 'ok-dir-locals-reload-for-all-buffers nil t)))

(add-hook 'emacs-lisp-mode-hook #'ok-dir-locals--add-reload-hook)
(add-hook 'lisp-data-mode-hook #'ok-dir-locals--add-reload-hook)

(provide 'subsys-dir-locals)
;;; subsys-dir-locals.el ends here
