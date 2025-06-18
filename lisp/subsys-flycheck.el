;;; subsys-flycheck.el --- Flycheck Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Flycheck subsystem.
;;
;;; Code:

(use-package flycheck
  :custom ((flycheck-python-mypy-executable (ok-file-expand-bin "mypy"))
           (flycheck-rst-executable (ok-file-expand-bin "rst2pseudoxml")))
  :hook (((emacs-lisp-mode lisp-data-mode) . flycheck-mode)
         (org-mode . flycheck-mode)))

;;; Textlint with Flycheck

(use-package flycheck
  :custom ((flycheck-textlint-config "default"))
  :ensure-system-package (textlint . "~/.config/emacs/bin/prepare-textlint")
  :config
  (defcustom flycheck-ok-textlint-config-dir "~/.config/textlint"
    "The directory storing textlint configuration files.")

  (defun flycheck-locate-config-file-textlint (filename checker)
    "Pick the correct `textlint' config file.
FILENAME may be set with buffer local variable,
`flycheck-textlint-config'. When FILENAME is given as an absolute path
and exists, it will be passed through.

FILENAME can also be a short string, in which case the config file of
the form '<lang>.<filename>.json' will be looked for in the directory
`flycheck-ok-textlint-config-dir'. The language LANG is autodetected
from the file content. If the file with that name exists, it will be
returned."
    (when (eq checker 'textlint)
      nil
      (if (file-exists-p (expand-file-name filename))
          (expand-file-name filename)
        (if-let* ((conf-dir (expand-file-name flycheck-ok-textlint-config-dir))
                  (fn (and (file-exists-p (expand-file-name filename conf-dir))
                           (expand-file-name filename conf-dir))))
            fn
          (let* ((lang (if (save-excursion
                             (goto-char (point-min))
                             (re-search-forward "[ぁ-んァ-ン一-龯]" nil t))
                           "ja" "en"))
                 (fn (expand-file-name (format "%s.%s.json" lang filename)
                                       conf-dir)))
            (when (file-exists-p fn)
              fn))))))

  (push #'flycheck-locate-config-file-textlint flycheck-locate-config-file-functions)
  (push '(org-mode . "@textlint/text") flycheck-textlint-plugin-alist))

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

;;; Misc.

(use-package flycheck-pos-tip
  :custom (flycheck-pos-tip-timeout 60)
  :config (flycheck-pos-tip-mode))

(provide 'subsys-flycheck)
;;; subsys-flycheck.el ends here
