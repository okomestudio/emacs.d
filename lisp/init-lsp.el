;;; init-lsp.el --- LSP  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :commands lsp

  :custom
  ;; TODO: Start the service on start.
  ;;
  ;; Run: docker container run --name explainshell --restart always \
  ;;          -p 5023:5000 -d spaceinvaderone/explainshell
  (lsp-bash-explainshell-endpoint "http://localhost:5023")

  (lsp-bash-highlight-parsing-errors t)
  (lsp-pylsp-configuration-sources ["flake8"])
  (lsp-pylsp-plugins-flake8-enabled t)
  (lsp-pylsp-plugins-pycodestyle-enabled nil)
  (lsp-pylsp-plugins-pydocstyle-add-ignore ["D100", "D103"])
  (lsp-pylsp-plugins-pydocstyle-convention "google")
  (lsp-pylsp-plugins-pydocstyle-enabled t)
  (lsp-pylsp-server-command '("~/.config/emacs/bin/pylsp"))
  (lsp-response-timeout 30)
  (lsp-sqls-workspace-config-path "root")
  (lsp-sqls-timeout 30)

  :ensure-system-package
  ((sqls . "go get github.com/lighttiger2505/sqls")
   (unified-language-server . "sudo npm i -g unified-language-server"))

  :hook
  ((dockerfile-mode . (lambda () (ts/lsp-mode-hook 'dockerfile-ls)))
   ;; (html-mode . lsp)
   (js-mode . (lambda () (ts/lsp-mode-hook 'jsts-ls)))
   (json-mode . (lambda () (ts/lsp-mode-hook 'json-ls)))
   (lsp-mode . lsp-enable-which-key-integration)
   (markdown-mode . lsp)
   (python-mode . (lambda () (ts/lsp-mode-hook 'pylsp)))
   (sh-mode . (lambda () (ts/lsp-mode-hook 'bash-ls)))
   (sql-mode . (lambda () (ts/lsp-mode-hook 'sqls)))
   (web-mode . (lambda () (ts/lsp-mode-hook 'html-ls)))
   (yaml-mode . (lambda () (ts/lsp-mode-hook 'yamlls))))

  :init
  (defun ts/lsp-mode-hook (server)
    (lsp-ensure-server server)
    (lsp)))

;; lsp-grammerly - lsp-mode and grammarly
;; https://github.com/emacs-grammarly/lsp-grammarly
(use-package lsp-grammarly
  :bind ("C-c g" . ts/check-grammar)
  :custom (lsp-grammarly-auto-activate nil)

  :ensure-system-package
  (unofficial-grammarly-language-server . "sudo npm i -g @emacs-grammarly/unofficial-grammarly-language-server")

  :init
  (use-package keytar
    :ensure-system-package (keytar . "sudo npm install -g @emacs-grammarly/keytar-cli"))

  (defun ts/grammarly--check-grammar (start end)
    (if (use-region-p)
        (let* ((parent-buffer (current-buffer))
               (tmp-filename (make-temp-file (concat (expand-file-name (buffer-file-name parent-buffer)) "-")))
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
                                 (lambda () (interactive) (ts/grammarly--finalize tmp-buffer parent-buffer start end)))
                  (local-set-key (kbd "C-c C-c")
                                 (lambda () (interactive) (ts/grammarly--finalize tmp-buffer parent-buffer start end)))

                  (lsp)
                  (lsp-grammarly-check-grammar))
              (progn
                (add-hook 'kill-emacs-hook (lambda () (ts/grammarly--clean-up-tmp-file tmp-filename))))
              ) ))
      (message "No region marked to check for grammar")))

  (defun ts/grammarly--clean-up-tmp-buffer (buffer)
    (when (get-buffer buffer)
      (set-buffer buffer)
      (save-buffer buffer)
      (kill-buffer-and-window)))

  (defun ts/grammarly--clean-up-tmp-file (tmp-filename)
    (if (file-exists-p tmp-filename)
        (delete-file tmp-filename)))

  (defun ts/grammarly--finalize (buffer parent-buffer start end)
    (set-buffer parent-buffer)
    (kill-region start end)
    (insert-buffer-substring buffer)
    (let* ((new-end (- (+ start (buffer-size buffer)) 1)))
      (ts/grammarly--clean-up-tmp-buffer buffer)
      (ts/grammarly--clean-up-tmp-file (buffer-file-name buffer))
      (set-buffer parent-buffer)
      (goto-char new-end)))

  (defun ts/check-grammar (start end)
    (interactive "r")
    (if (use-region-p)
        (ts/grammarly--check-grammar start end)
      (progn
        (mark-paragraph)
        (ts/grammarly--check-grammar (region-beginning) (region-end)))))
  )


;; lsp-ui - UI integration for lsp-mode
;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  ((lsp-ui-doc-delay 0.5)
   (lsp-ui-doc-position 'at-point)
   (lsp-ui-doc-use-webkit nil)) )

;; lsp-treemacs - lsp-mode and treemacs
;; https://github.com/emacs-lsp/lsp-treemacs
(use-package lsp-treemacs
  :bind ([f7] . lsp-treemacs-symbols)
  :commands lsp-treemacs-errors-list)

(provide 'init-lsp)
;;; init-lsp.el ends here
