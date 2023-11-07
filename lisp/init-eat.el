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
                (:exclude ".dir-locals.el" "*-tests.el"))))

(provide 'init-eat)
;;; init-eat.el ends here
