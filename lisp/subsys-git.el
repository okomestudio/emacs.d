;;; subsys-git.el --- Git Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the Git subsystem.
;;
;;; Code:

(use-package magit
  ;; TODO: Bind `magit-find-file'
  :custom ((magit-diff-refine-hunk t)
           (magit-format-file-function #'magit-format-file-nerd-icons)))

(use-package magit-prime
  ;; Speedup magit by priming caches before refresh.
  :hook (on-first-file-hook . magit-prime-mode))

(use-package magit-todos
  :after magit
  :hook (magit-mode . magit-todos-mode)
  :config
  (dolist (kw '("WIP"
                "TODO [#A]" "TODO [#B]" "TODO [#C]"
                "WIP [#A]" "WIP [#B]" "WIP [#C]"))
    (add-to-list 'magit-todos-keywords-list kw)))

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
