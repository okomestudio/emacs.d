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

;;; Textlint
(use-package flycheck
  :custom ((flycheck-textlint-config "default"))
  :ensure-system-package (textlint . "~/.config/emacs/bin/prepare-textlint")
  :config
  (defun flycheck-locate-config-file-textlint (filename checker)
    "Pick the correct `textlint' config file.
FILENAME can be set as a buffer local variable per document using
`flycheck-textlint-config'. When FILENAME is an absolute path and
exists, it will be used. Otherwise, the language is auto-detected and
the .json extension is postfixed to FILENAME before it will be searched
for in ~/.config/textlint."
    (when (eq checker 'textlint)
      (if (file-exists-p (expand-file-name filename))
          (expand-file-name filename)
        (let ((conf-dir (expand-file-name "~/.config/textlint")))
          (if (file-exists-p (expand-file-name filename conf-dir))
              (expand-file-name filename conf-dir)
            (let* ((lang (if (save-excursion
                               (beginning-of-buffer)
                               (re-search-forward "[ぁ-んァ-ン一-龯]" nil t))
                             "ja" "en"))
                   (filename (expand-file-name
                              (format "%s.%s.json" lang filename)
                              conf-dir)))
              (when (file-exists-p filename)
                filename)))))))

  (push #'flycheck-locate-config-file-textlint flycheck-locate-config-file-functions)
  (push '(org-mode . "@textlint/text") flycheck-textlint-plugin-alist))

;;; Aspell
(use-package flycheck-aspell
  :after (flycheck)
  :hook (org-mode . (lambda () (require 'flycheck-aspell))))

(use-package flycheck-aspell-org
  :straight (flycheck-aspell-org
             :host github
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
