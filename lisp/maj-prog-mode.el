;;; maj-prog-mode.el --- Prog Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the `prog-mode' and related utilities.
;;
;;; Code:

(use-package prog-mode
  :straight nil)

;;; EDITING

(use-package hideshow
  :straight nil
  :bind ( :map hs-minor-mode-map
          ("<backtab>" . hs-ok-toggle-hiding-all)
          ("C-c @ C-a" . hs-show-all)
          ("C-c @ C-t" . hs-hide-all))
  :hook (prog-mode . hs-minor-mode)
  :config
  (defun hs-ok-toggle-hiding-all ()
    "Toggle hiding of all program blocks."
    (interactive)
    (hs-life-goes-on
     (save-excursion
       (forward-sexp)
       (if (hs-already-hidden-p) (hs-show-all) (hs-hide-all)))))

  (defun hs-ok-toggle-hiding--ad (fun &rest _)
    "Advise `indent-for-tab-command' to add hiding toggle behavior."
    ;; (declare (debug t))
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
        (t (save-excursion (hs-show-block))
           (unless (eq (point) (end-of-line)) (forward-char))
           (beginning-of-defun)))))

  (advice-add #'indent-for-tab-command :around #'hs-ok-toggle-hiding--ad))

(use-package lisp
  :straight nil
  :bind (;; The following work in many prog modes, but defined in lisp.el:
         ("C-x n d" . narrow-to-defun)
         ("C-x n w" . widen)))

(use-package whitespace
  :straight nil
  :hook (whitespace-mode . whitespace-mode-ok--hook)
  :config
  (defun whitespace-mode-ok--hook ()
    (when (and (boundp 'aggressive-indent-mode) aggressive-indent-mode)
      (aggressive-indent-mode -1))
    (indent-tabs-mode 1)))

;;; FORMATTING
(use-package prettier-js
  :commands (prettier-js)
  :ensure-system-package (prettier . "npm install -g prettier"))

(provide 'maj-prog-mode)
;;; maj-prog-mode.el ends here
