;;; subsys-elfeed.el --- Elfeed Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Elfeed subsystem.
;;
;;; Code:

(use-package elfeed
  :custom ((elfeed-curl-max-connections 16)
           (elfeed-search-title-max-width 100)
           (elfeed-search-title-min-width 16)
           (elfeed-search-trailing-width 30)
           (elfeed-show-unique-buffers nil))
  :hook ((elfeed-show-mode . elfeed-ok-show-setup)
         (elfeed-search-update . elfeed-ok-search-setup))
  :config
  (load (ok-expand-etc "elfeed/init"))

  (setq elfeed-log-level 'info)  ; 'info or 'debug

  (set-face-attribute 'elfeed-search-title-face nil
                      :foreground (face-attribute 'shadow :foreground))
  (set-face-attribute 'elfeed-search-unread-title-face nil
                      :foreground (face-attribute 'default :foreground))

  (defun elfeed-ok-show-setup ()
    ;; Remove underline from zenkaku space
    (setq-local nobreak-char-display nil)

    ;; Adjust entry style via shr:
    (setq-local shr-folding-mode nil
                shr-indentation 4
                shr-max-image-proportion 0.8
                shr-use-fonts t
                shr-width 80))

  (defun elfeed-ok-search-setup ()
    (setq-local nobreak-char-display nil)))

(use-package elfeed-org
  :custom ((rmh-elfeed-org-files `(,(ok-expand-etc "elfeed/elfeed.org"))))
  :init
  ;; `elfeed-org' seems very expensive, so we want to lazy load. See
  ;; the `elfeed-org' function source to ensure that we take care of
  ;; the nested advice.
  (defvar elfeed-org-ok--initialized nil)

  (defun elfeed-org-ok--init (&rest _)
    (unless elfeed-org-ok--initialized
      (elfeed-org)
      ;; The advice added with `elfeed-org' won't run during this
      ;; initial call, so manually trigger `configure-elfeed'.
      (rmh-elfeed-org-process rmh-elfeed-org-files
                              rmh-elfeed-org-tree-id)
      (setq elfeed-org-ok--initialized t)))

  (advice-add #'elfeed :before #'elfeed-org-ok--init))

(provide 'subsys-elfeed)
;;; subsys-elfeed.el ends here
