;;; maj-prog-mode.el --- Prog Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the `prog-mode' and related utilities.
;;
;;; Code:

(use-package prog-mode
  :straight nil)

;;; EDITING
(use-package lisp
  :straight nil
  :bind (;; The following work in many prog modes, but defined in lisp.el:
         ("C-x n d" . narrow-to-defun)
         ("C-x n w" . widen)))

;;; FORMATTING
(use-package prettier-js
  :commands (prettier-js)
  :ensure-system-package (prettier . "npm install -g prettier"))

(provide 'maj-prog-mode)
;;; maj-prog-mode.el ends here
