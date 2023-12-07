;;; init-elfeed.el --- Elfeed  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package elfeed
  :bind
  (:map elfeed-search-mode-map
   ("C-c f 1" ("Select filter 1" .
               (lambda ()
                 (interactive)
                 (init-elfeed--switch-filter "@3-month-ago -news"))))
   ("C-c f 2" ("Select filter 2" .
               (lambda ()
                 (interactive)
                 (init-elfeed--switch-filter "@3-month-ago +news +hatena -hn"))))
   ("C-c f 3" ("Select filter 3" .
               (lambda ()
                 (interactive)
                 (init-elfeed--switch-filter "@3-month-ago +news -hatena +hn +thread -job"))))
   ("C-c f 4" ("Select filter 4" .
               (lambda ()
                 (interactive)
                 (init-elfeed--switch-filter "@3-month-ago +news -hatena +hn -thread +job"))))
   ("C-c c" ("Comments for URL" .
               (lambda (arg)
                 (interactive "P")
                 (let* ((entry (elfeed-search-selected :ignore-region))
                        (url (cdr (elfeed-entry-id entry))))
                   (if (string-match "news.ycombinator.com" url)
                       (ts/visit-url arg url)
                     (ts/visit-hatena-bookmark-comments arg url))))))

   :map elfeed-show-mode-map
   ("B" . init-elfeed--visit-hatena-bookmark-comments))

  :custom
  (elfeed-search-filter "@3-month-ago -news")
  (elfeed-show-unique-buffers nil)

  :hook
  (elfeed-show-mode . init-elfeed--elfeed-show-mode-hook)
  (elfeed-search-update . (lambda ()
                            (setq-local nobreak-char-display nil)
                            ;; (text-scale-set 0.5)
                            ))

  :preface
  (defun init-elfeed--elfeed-show-mode-hook ()
    (setq-local nobreak-char-display nil) ;; remove underline from zenkaku space

    ;; adjust entry style via shr:
    (setq-local shr-folding-mode nil
                shr-indentation 5
                shr-max-image-proportion 0.8
                shr-max-width 95
                shr-use-fonts t
                shr-width nil))

  (defun init-elfeed--visit-hatena-bookmark-comments (arg)
    (interactive "P")
    (elfeed-show-yank)
    (ts/visit-hatena-bookmark-comments arg))

  :config
  (defun init-elfeed--switch-filter (filter)
    (with-current-buffer (elfeed-search-buffer)
      (setf elfeed-search-filter filter)
      (elfeed-search-update :force)))

  (defhydra hydra-elfeed-show (:color pink :hint nil)
    "
^elfeed-show^
^^^^^^^^^^^^^------------------------------
_b_: visit the current entry in the browser
_B_: visit Hatena Bookmark comments for the entry
_q_: kill the buffer
_z_: zoom image (when on image)
"
    ("b" elfeed-show-visit)
    ("B" init-elfeed--visit-hatena-bookmark-comments)
    ("c" nil "cancel")
    ("q" elfeed-kill-buffer)
    ("z" shr-zoom-image))

  (define-key elfeed-show-mode-map "." 'hydra-elfeed-show/body))


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
  (elfeed-goodies/feed-source-column-width 28)
  (elfeed-goodies/tag-column-width 16)

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


(use-package elfeed-webkit
  :disabled
  :after elfeed)


(provide 'init-elfeed)
;;; init-elfeed.el ends here
