;;; 20-eat.el --- eat  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Emulate A Terminal, in a region, in a buffer and in Eshell.
;;
;;; Code:

(use-package eat
  :disabled
  :straight
  '(eat :type git
        :host codeberg
        :repo "akib/emacs-eat"
        :files ("*.el" ("term" "term/*.el") "*.texi"
                "*.ti" ("terminfo/e" "terminfo/e/*")
                ("terminfo/65" "terminfo/65/*")
                ("integration" "integration/*")
                (:exclude ".dir-locals.el" "*-tests.el")))

  :hook
  (eshell-post-command . (lambda ()
                           (sleep-for 0.2)
                           (end-of-buffer))))

;;; 20-eat.el ends here
