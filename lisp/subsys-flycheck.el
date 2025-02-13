;;; subsys-flycheck.el --- Flycheck Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Flycheck subsystem.
;;
;;; Code:

(use-package flycheck
  :custom ((flycheck-python-mypy-executable (locate-user-emacs-file "bin/mypy"))
           (flycheck-rst-executable (locate-user-emacs-file "bin/rst2pseudoxml")))
  :hook (((emacs-lisp-mode lisp-data-mode) . flycheck-mode)
         (org-mode . flycheck-mode)))

;;; Textlint
(use-package flycheck
  :custom ((flycheck-textlint-config "default"))
  :ensure-system-package (textlint . "~/.config/emacs/bin/prepare-textlint")
  :preface (ok-safe-local-variable-add flycheck-textlint-config stringp)
  :config
  (defun flycheck-locate-config-file-textlint (filename checker)
    (when (eq checker 'textlint)
      (let* ((conf-dir (convert-standard-filename "~/.config/textlint"))
             (filename (if (string= filename "default")
                           (if (save-excursion
                                 (beginning-of-line)
                                 (re-search-forward "[ぁ-んァ-ン一-龯]" nil t))
                               "ja.default.json"
                             "en.default.json")
                         filename))
             (path1 (expand-file-name filename conf-dir))
             (path2 (concat path1 ".json"))
             (path (or (and (file-exists-p path1) path1)
                       (and (file-exists-p path2) path2))))
        (when path
          path))))

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
