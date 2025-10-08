;;; maj-prog-mode.el --- The `prog-mode'  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the `prog-mode', a major mode for editing programming language
;; source code.
;;
;;; Code:

(use-package prog-mode
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

;;; Editing

(use-package elec-pair
  :hook (prog-mode . electric-pair-local-mode))

(use-package hideshow
  ;; The builtin package `hideshow' adds a minor mode to (un)hide portion of
  ;; code/comment blocks.
  :bind ( :map hs-minor-mode-map
          ("<backtab>" . hs-ok-toggle-hiding-all) ; shift + tab
          ("C-c @ C-a" . hs-show-all)
          ("C-c @ C-t" . hs-hide-all) )
  :hook (prog-mode . hs-minor-mode)
  :config
  (defun hs-ok-toggle-hiding-all ()
    "Toggle hiding of all program blocks."
    (interactive)
    (hs-life-goes-on
     (save-excursion
       ;; Move to a structure where `hs-already-hidden-p' can be used
       ;; to inspect the current toggle status.
       (forward-thing (cond ((derived-mode-p '(emacs-lisp-mode)) 'sexp)
                            (t 'defun)))
       (if (hs-already-hidden-p) (hs-show-all) (hs-hide-all)))))

  (defun hs-ok-toggle-hiding--ad (fun &rest _)
    "Advise `indent-for-tab-command' to add hiding toggle behavior."
    (if (and (boundp 'hs-minor-mode) (null hs-minor-mode))
        (apply fun _)
      (pcase (hs-already-hidden-p)
        (`nil (pcase (eq (point)
                         (save-excursion
                           (unless (eq (point) (end-of-line)) (forward-char))
                           (beginning-of-defun)
                           (point)))
                (`t (hs-hide-block)
                    (unless (eq (point) (end-of-line)) (forward-char))
                    (beginning-of-defun))
                (`nil (apply fun _))))
        (_ (save-excursion (hs-show-block))
           (unless (eq (point) (end-of-line)) (forward-char))
           (beginning-of-defun)))))

  (advice-add #'indent-for-tab-command :around #'hs-ok-toggle-hiding--ad))

(use-package lisp
  :bind (; The following work in many prog modes, but defined in lisp.el:
         ("C-x n d" . narrow-to-defun)
         ("C-x n w" . widen)))

(use-package whitespace
  :hook (whitespace-mode . whitespace-mode-ok--hook)
  :config
  (defun whitespace-mode-ok--hook ()
    (when (and (boundp 'aggressive-indent-mode) aggressive-indent-mode)
      (aggressive-indent-mode -1))
    (indent-tabs-mode 1)))

;;; Formatting

(use-package prettier-js
  :commands (prettier-js)
  :ensure-system-package (prettier . "npm install -g prettier"))

;;; Misc.

(use-package quickrun
  ;; Run command quickly.
  )

(provide 'maj-prog-mode)
;;; maj-prog-mode.el ends here
