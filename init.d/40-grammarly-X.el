;;; 40-grammarly.el --- Grammarly  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-grammarly
  :bind
  ("C-c g" . init-lsp--check-grammar)

  :custom
  (lsp-grammarly-auto-activate nil)

  :ensure-system-package
  (unofficial-grammarly-language-server . "npm i -g @emacs-grammarly/unofficial-grammarly-language-server")

  :init
  (use-package keytar
    :ensure-system-package
    (keytar . "npm install -g @emacs-grammarly/keytar-cli"))

  (defun init-lsp--grammarly--check-grammar (start end)
    (if (use-region-p)
        (let* ((parent-buffer (current-buffer))
               (tmp-filename (make-temp-file
                              (concat (expand-file-name (buffer-file-name parent-buffer)) "-")))
               (tmp-buffer (get-buffer-create tmp-filename)))
          (copy-to-buffer tmp-buffer start end)
          (with-current-buffer tmp-buffer
            (unwind-protect
                (progn
                  (set-visited-file-name tmp-filename)
                  (pop-to-buffer tmp-buffer)

                  (text-mode)
                  (use-local-map (copy-keymap text-mode-map))
                  (local-set-key (kbd "C-x k")
                                 (lambda ()
                                   (interactive)
                                   (init-lsp--grammarly--finalize tmp-buffer parent-buffer start end)))
                  (local-set-key (kbd "C-c C-c")
                                 (lambda ()
                                   (interactive)
                                   (init-lsp--grammarly--finalize tmp-buffer parent-buffer start end)))

                  (lsp)
                  (lsp-grammarly-check-grammar))
              (add-hook 'kill-emacs-hook
                        (lambda ()
                          (init-lsp--grammarly--clean-up-tmp-file tmp-filename))))))
      (message "No region marked to check for grammar")))

  (defun init-lsp--grammarly--clean-up-tmp-buffer (buffer)
    (when (get-buffer buffer)
      (set-buffer buffer)
      (save-buffer buffer)
      (kill-buffer-and-window)))

  (defun init-lsp--grammarly--clean-up-tmp-file (tmp-filename)
    (if (file-exists-p tmp-filename)
        (delete-file tmp-filename)))

  (defun init-lsp--grammarly--finalize (buffer parent-buffer start end)
    (set-buffer parent-buffer)
    (kill-region start end)
    (insert-buffer-substring buffer)
    (let* ((new-end (- (+ start (buffer-size buffer)) 1)))
      (init-lsp--grammarly--clean-up-tmp-buffer buffer)
      (init-lsp--grammarly--clean-up-tmp-file (buffer-file-name buffer))
      (set-buffer parent-buffer)
      (goto-char new-end)))

  (defun init-lsp--check-grammar (start end)
    (interactive "r")
    (if (use-region-p)
        (init-lsp--grammarly--check-grammar start end)
      (mark-paragraph)
      (init-lsp--grammarly--check-grammar (region-beginning) (region-end)))))

;;; 40-grammarly.el ends here
