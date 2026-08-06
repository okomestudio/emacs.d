;;; subsys-flycheck.el --- Flycheck  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the Flycheck subsystem.
;;
;;; Code:

(use-package flycheck
  :custom ((flycheck-python-mypy-executable (fs-emacs-bin "mypy"))
           (flycheck-rst-executable (fs-emacs-bin "rst2pseudoxml")))
  :config
  (which-key-add-key-based-replacements "C-c !" "flycheck-mode-map")
  :hook (((emacs-lisp-mode lisp-data-mode) . flycheck-mode)))

;;; Textlint Integration

(use-package flycheck
  :custom ((flycheck-textlint-config "default"))
  :ensure-system-package
  (textlint . "~/.config/emacs/bin/prepare-textlint")
  :config
  (defcustom flycheck-textlint-config-dir "~/.config/textlint"
    "The directory storing textlint configuration files."
    :group 'flycheck)

  (defun flycheck-locate-config-file-textlint (filename checker)
    "Returns the path to a `textlint' config file.
FILENAME identifies the textlint configuration file in JSON. It can take
one of the following forms:

  - an absolute path to a config file
  - a filename of a file in `flycheck-textlint-config-dir'
  - a short string such that '<lang>.<filename>.json' in
    `flycheck-textlint-config-dir' is a textlint config file for an
    auto-detected language

The function returns nil, if the file does not exists."
    (when (eq checker 'textlint)
      (cond
       ((file-exists-p (expand-file-name filename))
        (expand-file-name filename))
       ((file-exists-p (expand-file-name filename flycheck-textlint-config-dir))
        (expand-file-name filename flycheck-textlint-config-dir))
       (t
        (let* ((lang (if (save-excursion
                           (goto-char (point-min))
                           (re-search-forward "[ぁ-んァ-ン一-龯]" nil t))
                         "ja" "en"))
               (filename (expand-file-name (format "%s.%s.json" lang filename)
                                           flycheck-textlint-config-dir)))
          (when (file-exists-p filename)
            filename))))))

  (add-to-list 'flycheck-locate-config-file-functions #'flycheck-locate-config-file-textlint)
  (add-to-list 'flycheck-textlint-plugin-alist '(org-mode . "org")))

;;; LanguageTool

(use-package flycheck-languagetool
  :custom (flycheck-languagetool-language "en-US")
  :init
  (defun flycheck-languagetool--on-init ()
    (require 'ok-network)
    (ok-network-port-open-async
     "localhost" 8081
     (lambda (alive event)
       (if alive
           (progn
             (setq flycheck-languagetool-url "http://localhost:8081")
             (flycheck-languagetool-setup))
         (message "LanguageTool service unreachable (%s)" event)
         (let ((buf-name "*LanguageTool Launcher"))
           (message "Trying to start LanguageTool service...")
           (add-hook 'comint-mode-hook
                     (lambda ()
                       (when (string= (buffer-name) buf-name)
                         (remove-hook 'comint-output-filter-functions
                                      'ansi-color-process-output t))))
           (async-shell-command (fs-emacs-bin "docker-languagetool")
                                buf-name))))))
  :hook (text-mode . flycheck-languagetool--on-init))

;;; write good

(use-package flycheck
  ;; write good - Naive linter for English prose
  :disabled
  :ensure-system-package (write-good . "npm install -g write-good")
  :config
  (flycheck-define-checker write-good
    "The write-good prose checker."
    :command ("write-good" "--no-thereIs" "--parse" source-inplace)
    :standard-input nil
    :error-patterns ((warning
                      line-start
                      (file-name) ":" line ":" column ":" (message)
                      line-end))
    :modes (gfm-mode markdown-mode org-mode text-mode))
  (add-to-list 'flycheck-checkers 'write-good))

;;; Notification

(use-package flycheck-pos-tip
  :disabled
  :custom (flycheck-pos-tip-timeout 60)
  :hook (flycheck-mode . flycheck-pos-tip-mode))

(use-package flycheck-posframe
  :custom ((flycheck-posframe-border-use-error-face t)
           (flycheck-posframe-border-width 1))
  :config (flycheck-posframe-configure-pretty-defaults)
  :hook (flycheck-mode . flycheck-posframe-mode))

;;; Eglot

(use-package flycheck-eglot
  :after (flycheck eglot)
  :custom (flycheck-eglot-exclusive t))

;;; Misc.

(use-package flyover
  ;; A beautiful inline overlay for Flycheck
  :custom (flyover-use-theme-colors t)
  ;; :hook (flycheck-mode . flyover-mode)
  )

(provide 'subsys-flycheck)
;;; subsys-flycheck.el ends here
