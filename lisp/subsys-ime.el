;;; subsys-ime.el --- Input Method  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the input method subsystem.
;;
;;; Code:

(use-package emacs
  ;; Make blinking cursor sensitive to input method status.
  :config
  (setopt blink-cursor-interval 0.25
          blink-cursor-delay 0.25
          blink-cursor-blinks -1
          blink-cursor-alist '((box . hollow))
          cursor-type 'box)

  (defvar ok-cursor--base-color (face-attribute 'cursor :background))

  (defun ok-cursor--when-active ()
    (when-let* ((color (face-attribute 'warning :foreground)))
      (set-cursor-color color)))

  (defun ok-cursor--when-inactive ()
    (when-let* ((color ok-cursor--base-color))
      (set-cursor-color color)))

  (defun ok-cursor--on-buffer-switch ()
    (if current-input-method
        (ok-cursor--when-active)
      (ok-cursor--when-inactive)))

  (add-hook 'buffer-list-update-hook #'ok-cursor--on-buffer-switch)
  (add-hook 'input-method-activate-hook #'ok-cursor--when-active)
  (add-hook 'input-method-deactivate-hook #'ok-cursor--when-inactive))

(use-package mozc
  :bind (("C-z" . toggle-input-method) ("C-\\" . nil))
  :custom ((default-input-method "japanese-mozc")

           ;; NOTE(2025-03-12): `posframe' would be best, but it
           ;; frequently crashed with Cairo in a hard-to-debug way.
           ;; `overlay' runs sluggish. `echo-area' is sufficient and
           ;; stable, but far from the point of input. `popup' seems
           ;; to be the best compromise.
           (mozc-candidate-style (if (display-graphic-p) 'posframe 'echo-area))

           (mozc-leim-title "🇯🇵"))
  :commands (toggle-input-method)
  :config
  (when nil
    ;; NOTE(2025-05-26): Experiement with toggling IME.
    ;;
    ;; NOTE(2025-09-17): This extra toggle layer is unnecessary and often causes
    ;; conflicts with existing features. Remove on the next clean-up.
    (defun mozc-ok--im-deactivate (&rest r)
      (deactivate-input-method))
    (pcase-dolist (`(,feat ,fun) '((simple execute-extended-command)
                                   (emacs completing-read)
                                   (consult consult--read)))
      (with-eval-after-load feat
        (advice-add fun :before #'mozc-ok--im-deactivate)))))

(use-package mozc
  :if (and (eq system-type 'gnu/linux) (memq window-system '(pgtk)))
  :init
  ;; Setting this to non-nil uses the IME on OS:
  (setq pgtk-use-im-context-on-new-connection nil))

(use-package mozc-isearch
  :hook (;; NOTE(2025-03-18): This is an optimization to remove
         ;; initialization with `after-init-hook', which gets set up
         ;; when the `mozc-isearch' feature is required:
         (on-first-input . (lambda ()
                             (require 'mozc-isearch)
                             (mozc-isearch-workaround-setup)))

         (isearch-mode . mozc-isearch-ok--fix-ace-isearch))
  :config
  (defun mozc-isearch-ok--fix-ace-isearch (&rest _)
    "Disable `ace-isearch-mode' when `mozc' is active."
    ;; NOTE(2025-03-02): Mozc doesn't play nice with ace-isearch.
    (if (and (boundp 'mozc-mode) mozc-mode)
        (ace-isearch-mode -1)
      (ace-isearch-mode 1))))

;;; Posframe

(use-package mozc-cand-posframe
  ;; For `posframe', this package may be the simplest option.
  :disabled
  :if (display-graphic-p)
  :after mozc
  :hook ((enable-theme-functions . mozc-posframe-ok--theme))
  :init (require 'mozc-cand-posframe)
  :config
  (defun mozc-posframe-ok--theme (theme)
    (require 'corfu)
    (let* ((face-base 'corfu-default)
           (face-current 'corfu-current)
           (foreground (face-attribute face-base :foreground))
           (background (face-attribute face-base :background)))
      (dolist (face '(mozc-cand-posframe-normal-face
                      mozc-cand-posframe-footer-face))
        (set-face-attribute face nil
                            :foreground foreground
                            :background background
                            :inherit face-base))
      (set-face-attribute 'mozc-cand-posframe-focused-face nil
                          :foreground (face-attribute face-current :foreground)
                          :background (face-attribute face-current :background)
                          :inherit face-current))))

(use-package mozc-posframe
  ;; This might work, but not out of box.
  :if (display-graphic-p)
  :hook ((on-first-input . mozc-posframe-initialize)
         (enable-theme-functions . mozc-posframe-ok--theme))
  :config
  (defun mozc-posframe-ok--theme (theme)
    (require 'corfu)
    (let* ((scale (pcase (frame-parameter nil 'background-mode)
                    ('dark 1.07) ('light 0.93)))
           (face-b 'corfu-default)
           (fg-b (face-attribute face-b :foreground))
           (bg-b (ok-face-color-scale
                  (or (when (stringp (face-attribute face-b :background))
                        (face-attribute face-b :background))
                      (face-attribute 'default :background))
                  scale))
           (face-c 'corfu-current)
           (fg-c (face-attribute face-c :foreground))
           (bg-c (ok-face-color-scale
                  (or (when (stringp (face-attribute face-c :background))
                        (face-attribute face-c :background))
                      (face-attribute 'default :background))
                  scale)))
      (dolist (face '(mozc-cand-overlay-even-face
                      mozc-cand-overlay-odd-face
                      mozc-cand-overlay-description-face
                      mozc-cand-overlay-footer-face))
        (set-face-attribute face nil
                            :foreground fg-b :background bg-b :inherit face-b))
      (set-face-attribute 'mozc-cand-overlay-focused-face nil
                          :foreground fg-c :background bg-c :inherit face-c))))

(provide 'subsys-ime)
;;; subsys-ime.el ends here
