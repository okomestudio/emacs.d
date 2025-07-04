;;; subsys-flycheck.el --- Flycheck  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the Flycheck subsystem.
;;
;;; Code:

(require 'straight)

(straight-override-recipe
 '(flycheck :type git :host github :repo "flycheck/flycheck"
            :fork ( :branch "fix-line-prefix" )))

(use-package flycheck
  :custom ((flycheck-python-mypy-executable (ok-file-expand-bin "mypy"))
           (flycheck-rst-executable (ok-file-expand-bin "rst2pseudoxml")))
  :hook (((emacs-lisp-mode lisp-data-mode) . flycheck-mode))
  :config
  (which-key-add-key-based-replacements "C-c !" "flycheck-mode-map"))

;;; Textlint with Flycheck

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

  (add-to-list 'flycheck-locate-config-file-functions
               #'flycheck-locate-config-file-textlint)
  (add-to-list 'flycheck-textlint-plugin-alist '(org-mode . "org")))

;;; Aspell

(use-package flycheck-aspell-org
  :straight (flycheck-aspell-org :host github
                                 :repo "okomestudio/flycheck-aspell-org.el")
  :config
  (add-to-list 'flycheck-checkers 'org-aspell-dynamic)
  (flycheck-add-next-checker 'org-aspell-dynamic '(t . textlint)))

;;; Notification

(use-package flycheck-pos-tip
  :disabled
  :custom (flycheck-pos-tip-timeout 60)
  :hook (flycheck-mode . flycheck-pos-tip-mode))

(use-package flycheck-posframe
  :custom ((flycheck-posframe-border-use-error-face t)
           (flycheck-posframe-border-width 1))
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config (flycheck-posframe-configure-pretty-defaults))

;;; Eglot

(use-package flycheck-eglot
  :after (flycheck eglot)
  :custom (flycheck-eglot-exclusive t))

;;; Misc.

(defcustom subsys-flycheck-mode 'default
  "Flychcek mode in effect, default or eglot."
  :type 'symbol)

(defun subsys-flycheck--init-org-mode ()
  "Initialize flycheck in `org-mode'."
  (pcase subsys-flycheck-mode
    ('default
     (require 'flycheck-aspell-org)
     (add-to-list 'flycheck-checkers 'org-aspell-dynamic)
     (flycheck-add-next-checker 'org-aspell-dynamic '(t . textlint))
     (flycheck-mode 1))
    ('eglot
     (require 'flycheck-eglot)
     (setq-local flycheck-disabled-checkers '(org-aspell-dynamic textlint))
     (eglot-ensure)
     (flycheck-eglot-mode 1))))

(add-hook 'org-mode-hook #'subsys-flycheck--init-org-mode)

(provide 'subsys-flycheck)
;;; subsys-flycheck.el ends here
