;;; 05-themes-org.el --- themes-org  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure Org themes, staging before making it into a theme.
;;
;;; Code:

(defvar ok-faces-font-family-outline "URW Classico"
  "Font for outlines.")

(defvar ok-faces-font-family-outline-ja "Noto Sans CJK JP"
  "Font for outlines in Japanese.")

(defface ok-faces-outline '((t :inherit 'default))
  "Face for outlines.
Use when contrast with non-outline contenst is desired."
  :group 'ok)

(defvar ok-faces-org-fixed-pitch-faces
  '(font-lock-builtin-face
    font-lock-comment-delimiter-face
    font-lock-comment-face
    font-lock-constant-face
    font-lock-doc-face
    font-lock-function-name-face
    font-lock-keyword-face
    font-lock-negation-char-face
    font-lock-preprocessor-face
    font-lock-regexp-grouping-backslash
    font-lock-regexp-grouping-construct
    font-lock-string-face
    font-lock-type-face
    font-lock-variable-name-face
    org-block
    org-block-begin-line
    org-block-end-line
    org-checkbox
    org-code
    org-document-info
    org-document-info-keyword
    org-drawer
    org-formula
    org-indent
    org-latex-and-related
    org-meta-line
    org-modern-bracket-line
    org-modern-tag
    org-property-value
    org-special-keyword
    ;; org-table
    org-verbatim)
  "Fixed-pitch faces in Org mode.")

(defvar ok-faces-org-outline-faces
  '((org-level-1 . '(:height 1.24))
    (org-level-2 . '(:height 1.12))
    (org-level-3 . '(:height 1.00))
    (org-level-4 . '(:height 0.90))
    (org-level-5 . '(:height 0.90))
    (org-level-6 . '(:height 0.90))
    (org-level-7 . '(:height 0.90))
    (org-level-8 . '(:height 0.90))
    (org-document-title . '(:height 1.24)))
  "Base outlines faces used in Org mode.")

(use-package faces
  :straight nil
  :init (push '("URW Classico" . 1.28) face-font-rescale-alist))

(use-package org-faces
  :straight nil
  :after (org)
  :config
  (let ((fontset "fontset-urw classic")
        frame)
    ;; Create fontset.
    (ok-faces-create-fontset fontset
                             ok-faces-font-family-outline
                             ok-faces-font-family-outline-ja
                             frame)
    (set-face-attribute 'ok-faces-outline frame :font fontset :fontset fontset)

    ;; Set faces.
    (dolist (it ok-faces-org-outline-faces)
      (let* ((face (car it))
             (prop (cdr it))
             (height (or (car (cdr (assoc :height prop)))
                         (face-attribute face :height nil t)))
             (foreground (face-attribute face :foreground nil t))
             (weight (face-attribute face :weight nil t))
             (inherit 'ok-faces-outline))
        (set-face-attribute face frame
                            :height height
                            :foreground foreground
                            :weight weight
                            :inherit inherit)))

    (set-face-attribute 'org-drawer frame
                        :foreground (face-attribute 'shadow :foreground))

    (set-face-attribute 'org-link frame :weight 'normal)
    (with-eval-after-load 'org-ref
      (set-face-attribute 'org-ref-cite-face nil :weight 'normal)))

  (defun ok-faces-org--remap-to-mixed-pitch ()
    (face-remap-add-relative 'default :inherit 'variable-pitch)
    (dolist (face ok-faces-org-fixed-pitch-faces)
      (face-remap-add-relative face :inherit 'fixed-pitch)))

  (defun ok-faces-org--handle-text-scale-mode ()
    (let ((height (ok-faces-text-scale-mode-height)))
      (when height
        (dolist (face (append ok-faces-org-fixed-pitch-faces
                              ok-faces-org-outline-faces))
          (if (consp face)
              (setq face (car face)))
          (face-remap-add-relative face :height height)))))

  (add-hook 'org-mode-hook
            (lambda ()
              (ok-faces-org--remap-to-mixed-pitch)
              (ok-faces-org--handle-text-scale-mode)
              ;; (force-window-update (current-buffer))
              )
            91))

;; Local Variables:
;; nameless-aliases: (("" . "ok-faces-org"))
;; End:
;;; 05-themes-org.el ends here
