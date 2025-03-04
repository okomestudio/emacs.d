;;; themes-modeline.el --- Modeline Theme  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up modeline theme.
;;
;;; Code:

(use-package doom-modeline
  ;; A fancy and fast mode-line inspired by minimalism design.
  :custom ((doom-modeline-buffer-encoding nil)
           (doom-modeline-buffer-file-name-style 'buffer-name)
           (doom-modeline-check-simple-format t)
           (doom-modeline-height 1)
           (doom-modeline-minor-modes t)
           (doom-modeline-vcs-max-length 12)
           (mode-line-percent-position nil)
           (doom-modeline-checker-simple-format t))
  :hook (enable-theme-functions . (lambda (_) (doom-modeline-mode)))
  :config
  (doom-modeline-def-modeline 'lsp-full
    '(bar workspace-name window-number modals matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state persp-name battery grip irc mu4e gnus github debug repl input-method minor-modes indent-info buffer-encoding major-mode vcs lsp misc-info time))

  (dolist (mode '(python-ts-mode python-sql-ts-mode))
    (add-to-list 'doom-modeline-mode-alist `(,mode . lsp-full))))

(use-package minions
  ;; A minor-mode menu for the mode line.
  :custom (minions-direct '(projectile-mode))
  :hook (enable-theme-functions . (lambda (_) (minions-mode))))

(provide 'themes-modeline)
;;; themes-modeline.el ends here
