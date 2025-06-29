;;; subsys-flycheck.el --- Flycheck  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the Flycheck subsystem.
;;
;;; Code:

(use-package flycheck
  :straight (flycheck :fork ( :branch "fix-line-prefix" ))
  :custom ((flycheck-python-mypy-executable (ok-file-expand-bin "mypy"))
           (flycheck-rst-executable (ok-file-expand-bin "rst2pseudoxml")))
  :hook (((emacs-lisp-mode lisp-data-mode) . flycheck-mode)
         (org-mode . flycheck-mode))
  :config
  (which-key-add-key-based-replacements "C-c !" "flycheck-mode-map"))

;;; Textlint with Flycheck

(use-package flycheck
  :straight nil               ; not the main flycheck config
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
  (add-to-list 'flycheck-textlint-plugin-alist '(org-mode . "@textlint/text")))

;;; Aspell

(use-package flycheck-aspell
  :after (flycheck)
  :hook (org-mode . (lambda () (require 'flycheck-aspell))))

(use-package flycheck-aspell-org
  :straight (flycheck-aspell-org :host github
                                 :repo "okomestudio/flycheck-aspell-org.el")
  :after (flycheck-aspell)
  :demand t
  :config (flycheck-add-next-checker 'org-aspell-dynamic 'textlint))

;;; Notification

(use-package flycheck-pos-tip
  :disabled
  :custom (flycheck-pos-tip-timeout 60)
  :hook (flycheck-mode . flycheck-pos-tip-mode))

(use-package flycheck-posframe
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config (flycheck-posframe-configure-pretty-defaults))

(provide 'subsys-flycheck)
;;; subsys-flycheck.el ends here
