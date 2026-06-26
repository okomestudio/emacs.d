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

  ;; FIX(2025-09-20): This doesn't work with `vertico-posframe'. Look for a way
  ;; to change the cursor color in the child frame.
  (defvar ok-cursor-color-default (face-attribute 'cursor :background))
  (defvar ok-cursor-color-im-active (face-attribute 'warning :foreground))

  (defun ok-cursor--when-active ()
    (set-cursor-color ok-cursor-color-im-active))

  (defun ok-cursor--when-inactive ()
    (set-cursor-color ok-cursor-color-default))

  (defun ok-cursor--update ()
    (with-current-buffer (current-buffer)
      (if current-input-method
          (ok-cursor--when-active)
        (ok-cursor--when-inactive))))

  (defun ok-cursor--update-posframe (fun &rest args)
    ;; TODO(2026-06-25): `args' contain :cursor key-value pair, which actually
    ;; only controls cursor-type. The `posframe' module needs to be patched to
    ;; support cursor-color. Do that first. Then, it can be passed as a frame
    ;; parameter and reflected on `postframe-show'.
    (apply fun args))

  ;; (advice-add 'posframe-show :around #'ok-cursor--update-posframe)

  (add-hook 'buffer-list-update-hook #'ok-cursor--update)
  (add-hook 'input-method-activate-hook #'ok-cursor--when-active)
  (add-hook 'input-method-deactivate-hook #'ok-cursor--when-inactive))

(use-package mozc
  :bind (("C-z" . toggle-input-method) ("C-\\" . nil))
  :custom ((default-input-method "japanese-mozc")

           ;; NOTE(2025-03-12): `posframe' would be best, but it frequently
           ;; crashed with Cairo in a hard-to-debug way. `overlay' is sluggish.
           ;; `echo-area' is sufficient and stable, but far from the point of
           ;; input. `popup' seems to be the best compromise?
           (mozc-candidate-style 'echo-area)

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

;;; Posframe

(use-package mozc-cand-posframe
  ;; For `posframe', this package may be the simplest option.
  :disabled
  :if (display-graphic-p)
  :after mozc
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
                          :inherit face-current)))

  :hook ((enable-theme-functions . mozc-posframe-ok--theme)))

(use-package mozc-posframe
  ;; This might work, but not out of box.
  :if (display-graphic-p)
  :custom (mozc-candidate-style 'posframe)
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
                          :foreground fg-c :background bg-c :inherit face-c)))

  :hook ((on-first-input . mozc-posframe-initialize)
         (enable-theme-functions . mozc-posframe-ok--theme)))

(use-package migemo
  :disabled
  :custom ((migemo-command "cmigemo")
           (migemo-options '("-q" "--emacs"))
           (migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
           (migemo-user-dictionary nil)
           (migemo-regex-dictionary nil)
           (migemo-coding-system 'utf-8-unix))
  :ensure-system-package (cmigemo . "sudo apt install cmigemo")
  :init (require 'migemo)
  :config (migemo-init))

(provide 'subsys-ime)
;;; subsys-ime.el ends here
