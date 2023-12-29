;;; init-lsp.el --- LSP  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package lsp-mode
  :commands lsp

  :custom
  (lsp-diagnostics-provider :auto)
  (lsp-log-io nil) ;; set to t for debugging
  (lsp-response-timeout 30)
  (lsp-use-plists t)

  :hook
  ((dockerfile-mode . (lambda () (init-lsp-lsp-mode-hook 'dockerfile-ls)))
   (json-mode . (lambda () (init-lsp-lsp-mode-hook 'json-ls)))
   (markdown-mode . lsp)
   (yaml-mode . (lambda () (init-lsp-lsp-mode-hook 'yamlls))))

  :preface
  (defun init-lsp-lsp-mode-hook (server)
    (lsp-ensure-server server)
    (lsp))

  (put 'lsp-disabled-clients 'safe-local-variable #'listp)

  :init
  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

  :config
  (setq lsp-headerline-arrow "➤"))


(use-package lsp-ui
  ;; UI integration for lsp-mode.
  ;;
  ;; Note that unfocusing the doc frame often leads to unresponsiveness.
  ;; Pressing an arrow key goes out of the state. See
  ;; https://github.com/emacs-lsp/lsp-ui/issues/715.
  ;;
  :commands lsp-ui-mode

  :bind
  (:map lsp-ui-mode-map
   ("C-F" . lsp-ui-doc-focus-frame)

   :map lsp-ui-doc-frame-mode-map
   ("q" . lsp-ui-doc-unfocus-frame))

  :custom
  (lsp-ui-doc-border "black")
  (lsp-ui-doc-delay 0.2)
  (lsp-ui-doc-max-height 10)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-text-scale-level -1.0)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit nil)
  (lsp-ui-sideline-delay 1.0)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover t))


(use-package lsp-treemacs
  :bind ([f7] . lsp-treemacs-symbols)
  :commands lsp-treemacs-errors-list)


(use-package lsp-grammarly
  :disabled

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


(provide 'init-lsp)
;;; init-lsp.el ends here
