;;; subsys-git.el --- Git Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the Git subsystem.
;;
;;; Code:

;;; Magit

(use-package magit
  ;; TODO: Bind `magit-find-file'
  :custom ((magit-diff-refine-hunk t)
           (magit-format-file-function #'magit-format-file-nerd-icons))
  :config
  ;; NOTE(2026-07-05): The explicit invocation of `global-git-commit-mode'
  ;; shouldn't be necessary but fixes the issue of commit message editing buffer
  ;; not activating `git-commit-mode':
  (require 'git-commit)
  (global-git-commit-mode 1)

  (add-hook 'magit-status-sections-hook #'magit-insert-worktrees 99))

(use-package magit-difftastic
  ;; NOTE(2026-06-14): The mode activation somehow fails. Revisit installation
  ;; and configuration.
  :disabled
  :after magit
  :ensure-system-package (difft . "sudo snap install difftastic") ; see hostconf role
  :config (magit-difftastic-mode +1))

(use-package magit-prime
  ;; Speedup magit by priming caches before refresh.
  :hook (on-first-file-hook . magit-prime-mode))

(use-package magit-todos
  :after magit
  :bind ( :map magit-todos-section-map
          ("R" . magit-todos-update) )
  :custom ((magit-todos-submodule-list t)
           (magit-todos-update nil))
  :config
  (dolist (kw '("TODO" "TODO [#A]" "TODO [#B]" "TODO [#C]"
                "WIP" "WIP [#A]" "WIP [#B]" "WIP [#C]"))
    (add-to-list 'magit-todos-keywords-list kw))

  :hook (magit-mode . magit-todos-mode))

;;; Blame

(use-package blamer
  ;; Git blame plugin.
  :bind (("s-b" . blamer-show-posframe-commit-info))
  :custom ((blamer-idle-time 0.3)
           (blamer-min-offset 70))
  :custom-face (blamer-face ((t :foreground "#7a88cf"
                                :background unspecified
                                :height 100
                                :italic t))))

;;; Diff Visualization

(use-package diff-hl
  ;; Uses margin.
  :init (global-diff-hl-mode))

(use-package git-gutter
  ;; Uses fringe.
  :disabled
  :hook (on-first-buffer . global-git-gutter-mode)
  :config
  (dolist (face '(git-gutter:separator
                  git-gutter:modified
                  git-gutter:added
                  git-gutter:deleted
                  git-gutter:unchanged))
    (set-face-attribute face nil
                        :height (round (* 0.8
                                          (face-attribute 'default :height))))))

;;; Misc.

(use-package browse-at-remote
  ;; Browse target page on github/bitbucket from emacs buffers.
  ;;
  ;; TODO(2025-06-07): Force the use of an external browser?
  :bind ( ("C-c g g" . browse-at-remote) ))

(provide 'subsys-git)
;;; subsys-git.el ends here
