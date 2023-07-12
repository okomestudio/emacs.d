;;; init-elfeed.el --- Elfeed  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package elfeed
  :custom
  (elfeed-search-filter "@3-month-ago ")

  :hook
  (elfeed-show-mode . init-elfeed--elfeed-show-mode-hook)

  :preface
  (defun init-elfeed--elfeed-show-mode-hook ()
    ;; Adjust the feed article page style here:
    (setq-local shr-width nil
                shr-max-width nil
                shr-use-fonts t)
    (toggle-truncate-lines +1)))


(use-package elfeed-org
  :custom
  (rmh-elfeed-org-files (init-elfeed--rmh-elfeed-org-files))

  :preface
  (defun init-elfeed--rmh-elfeed-org-files ()
    "Read elfeed-org files under conf.d."
    (let ((conf-d-dir (concat user-emacs-directory "conf.d/")))
      (if (file-exists-p conf-d-dir)
          (directory-files conf-d-dir t "^elfeed.*\\.org$"))))

  :config
  (elfeed-org))


(use-package elfeed-goodies
  :custom
  (elfeed-goodies/entry-pane-position 'bottom)
  (elfeed-goodies/entry-pane-size 0.8)

  :config
  (elfeed-goodies/setup)

  ;; Keymap hack to prevent elfeed-goodies from stealing M-v. See
  ;; https://emacs.stackexchange.com/a/65138/599.
  (defvar init-elfeed--map (make-keymap))
  (define-minor-mode init-elfeed--mode
    "Dummy minor mode for keymap hack."
    :init-value t
    :global t
    :keymap init-elfeed--map)
  (add-to-list 'emulation-mode-map-alists `((init-elfeed--mode) . ,init-elfeed--map))
  (define-key init-elfeed--map (kbd "M-v") 'scroll-down-command))


(provide 'init-elfeed)
;;; init-elfeed.el ends here
