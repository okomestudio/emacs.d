;;; init-elfeed.el --- Elisp  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package elfeed
  :custom
  (elfeed-search-filter "@3-month-ago ")

  :hook
  (elfeed-show-mode . ts/elfeed-show-mode-hook)

  :preface
  (defun ts/elfeed-show-mode-hook ()
    (setq-local shr-width nil
                shr-max-width nil
                shr-use-fonts t)
    (toggle-truncate-lines +1)))


(use-package elfeed-org
  :custom
  (rmh-elfeed-org-files (ts/rmh-elfeed-org-files))

  :preface
  (defun ts/rmh-elfeed-org-files ()
    (let ((conf-d-dir (concat user-emacs-directory "conf.d/")))
      (if (file-exists-p conf-d-dir)
          (directory-files conf-d-dir t "^elfeed.*\\.org$"))))

  :config
  (elfeed-org))


(provide 'init-elfeed)
;;; init-elfeed.el ends here
