;;; maj-elfeed.el --- Elfeed  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure Elfeed.
;;
;;; Code:

(require 'subsys-readers)

(use-package elfeed
  :bind ( :map reader-app-prefix-map
          ("e" . elfeed) )
  :custom ((elfeed-curl-max-connections 16)
           (elfeed-search-title-max-width 100)
           (elfeed-search-title-min-width 16)
           (elfeed-search-trailing-width 30)
           (elfeed-show-unique-buffers nil))
  :hook ((elfeed-show-mode . elfeed-ok--show-setup)
         (elfeed-search-update . elfeed-ok--search-setup)
         (enable-theme-functions . elfeed-ok--theme-hook))
  :config
  (setq elfeed-log-level 'info) ; 'info or 'debug

  (defun elfeed-ok--show-setup ()
    ;; Remove underline from zenkaku space
    (setq-local nobreak-char-display nil)

    ;; Adjust entry style via shr:
    (setq-local shr-folding-mode nil
                shr-indentation 4
                shr-max-image-proportion 0.8
                shr-use-fonts t
                shr-width 80))

  (defun elfeed-ok--search-setup ()
    (setq-local nobreak-char-display nil))

  (defun elfeed-ok--theme-hook (theme)
    (set-face-attribute 'elfeed-search-title-face nil
                        :foreground (face-attribute 'shadow :foreground))
    (set-face-attribute 'elfeed-search-unread-title-face nil
                        :foreground (face-attribute 'default :foreground)))

  (load (ok-file-expand-etc "elfeed/init")))

(use-package elfeed-org
  :custom ((rmh-elfeed-org-files `(,(ok-file-expand-etc "elfeed/elfeed.org"))))
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

(use-package elfeed-tube
  ;; NOTE(2025-07-23): `elfeed-tube-add-feeds' doesn't seem to work. One problem
  ;; is `aio' bug, which is apparently fixed in a fork (see the straight recipe
  ;; for emacs-aio). Troubleshoot this step before going further.
  :disabled

  :bind ( :map elfeed-show-mode-map
          ("Y" . elfeed-tube-fetch)
          ([remap save-buffer] . elfeed-tube-save)
          :map elfeed-search-mode-map
          ("Y" . elfeed-tube-fetch)
          ([remap save-buffer] . elfeed-tube-save) )
  :config (elfeed-tube-setup))

(provide 'maj-elfeed)
;;; maj-elfeed.el ends here
