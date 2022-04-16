;;; init-elfeed.el --- Elisp  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package elfeed
  :hook (elfeed-show-mode . ts/elfeed-show-mode-hook)
  :init
  (defun ts/elfeed-show-mode-hook ()
    (setq-local shr-width nil
                shr-max-width nil)
    (text-scale-set 2.0)
    (toggle-truncate-lines +1)))

(use-package elfeed-org
  :config (elfeed-org)
  :custom (rmh-elfeed-org-files (ts/rmh-elfeed-org-files))
  :init
  (defun ts/rmh-elfeed-org-files ()
    (let ((conf-d-dir (concat user-emacs-directory "conf.d/")))
      (if (file-exists-p conf-d-dir) (directory-files conf-d-dir t "^elfeed.*\\.org$")))))

(provide 'init-elfeed)
;;; init-elfeed.el ends here
