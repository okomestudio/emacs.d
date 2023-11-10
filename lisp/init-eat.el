;;; init-eat.el --- eat  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Emulate A Terminal, in a region, in a buffer and in Eshell.
;;
;;; Code:

(use-package eat
  :straight
  '(eat :type git
        :host codeberg
        :repo "akib/emacs-eat"
        :files ("*.el" ("term" "term/*.el") "*.texi"
                "*.ti" ("terminfo/e" "terminfo/e/*")
                ("terminfo/65" "terminfo/65/*")
                ("integration" "integration/*")
                (:exclude ".dir-locals.el" "*-tests.el")))

  :init
  (add-hook 'eshell-post-command-hook
            (lambda ()
              (sleep-for 0.2)
              (end-of-buffer)))

  ;; :init
  ;; The following might be necessary for performance. See
  ;; https://www.reddit.com/r/emacs/comments/17nl7cw/comment/k7u1ueu/
  ;;
  ;; (setq process-adaptive-read-buffering nil)
  ;; (setq read-process-output-max (* 4 1024 1024))
  )

(provide 'init-eat)
;;; init-eat.el ends here
