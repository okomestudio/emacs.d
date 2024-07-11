;;; 10-lsp-grammarly.el --- lsp-grammarly  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure Grammarly and related utilities.
;;
;;; Code:

(use-package lsp-grammarly
  :bind (nil
         :map text-mode-map
         ("C-c g" . grammarly-edit-open))

  :custom
  (lsp-grammarly-auto-activate nil)

  :config
  ;; GRAMMARLY EDIT BUFFER
  ;;
  ;; This implements Org Src-like buffer to use Grammarly.
  ;;
  (defconst grammarly-edit-buffer-name-prefix "*Grammarly "
    "Buffer name prefix for Grammarly edit buffer.")

  (defconst grammarly-edit-buffer-name-postfix "*"
    "Buffer name postfix for Grammarly edit buffer.")

  (defun grammarly-edit--open (start end)
    "Actually open and run grammarly-ls on Grammarly edit buffer."
    (let* ((parent-buffer (current-buffer))
           (parent-buffer-file-name (file-name-nondirectory
                                     (buffer-file-name parent-buffer)))
           (buffer-name (format "%s%s:%d:%d%s"
                                grammarly-edit-buffer-name-prefix
                                parent-buffer-file-name start end
                                grammarly-edit-buffer-name-postfix))
           (edit-buffer-file-name (format "grammarly-%s-%d-%d~"
                                          parent-buffer-file-name start end))
           (edit-buffer-file-name (file-truename edit-buffer-file-name))
           (edit-buffer (generate-new-buffer buffer-name)))
      (copy-to-buffer edit-buffer start end)
      (with-current-buffer edit-buffer
        (unwind-protect
            (progn
              (setq buffer-file-name edit-buffer-file-name)
              (pop-to-buffer edit-buffer)
              (set-buffer-modified-p nil)
              (text-mode)

              (defun grammarly-edit--commit-change ()
                (interactive)
                (save-buffer)
                (with-current-buffer parent-buffer
                  (kill-region start end)
                  (insert-buffer-substring edit-buffer))
                (let ((new-end (1- (+ start (buffer-size edit-buffer)))))
                  (kill-buffer)
                  (set-buffer parent-buffer)
                  (goto-char new-end)))

              (defun grammarly-edit--abort ()
                (interactive)
                (save-buffer)
                (kill-buffer)
                (set-buffer parent-buffer))

              (use-local-map (copy-keymap text-mode-map))
              (local-set-key (kbd "C-c '") #'grammarly-edit--commit-change)
              (local-set-key (kbd "C-x k") #'grammarly-edit--abort)

              (setq-local lsp-disabled-clients '(ltex-ls)
                          lsp-headerline-breadcrumb-enable nil)
              (lsp)

              (setq header-line-format
	                  (substitute-command-keys
                     "Edit, then exit with `C-c \'' or abort with `C-x k'.")))
          (add-hook 'kill-buffer-hook #'grammarly-edit--on-kill-buffer)))))

  (defun grammarly-edit--on-kill-buffer ()
    (when (string-match-p (format "^%s.*" (regexp-quote grammarly-edit-buffer-name-prefix))
                          (buffer-name))
      (if (file-exists-p buffer-file-name)
          (delete-file buffer-file-name))))

  (defun grammarly-edit--on-kill-emacs ()
    (dolist (buffer (buffer-list))
      (when (string-match-p (format "^%s.*" (regexp-quote grammarly-edit-buffer-name-prefix))
                            (buffer-name buffer))
        ;; Explicitly kill here so that the kill-buffer-hook gets run
        (kill-buffer buffer))))
  (add-hook 'kill-emacs-hook #'grammarly-edit--on-kill-emacs)

  (defun grammarly-edit-open (start end)
    "Open Grammarly edit buffer over region from START to END."
    (interactive "r")
    (if (use-region-p)
        (grammarly-edit--open start end)
      (mark-paragraph)
      (grammarly-edit--open (region-beginning) (region-end)))))

;; Local Variables:
;; nameless-aliases: (("" . "grammarly-edit"))
;; End:
;;; 10-lsp-grammarly.el ends here
