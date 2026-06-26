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

  (defvar mozc-cursor-color-off (face-attribute 'cursor :background))
  (defvar mozc-cursor-color-on (face-attribute 'warning :foreground))
  (defvar mozc-cursor-current-input-mode nil
    "Symbol indicating the current input mode.
For 'japanese-mozc', it is one of direct, hiragana, katakana, half_ascii,
full_ascii, and half_katakana.

This is set to nil when input method is not active.")
  (make-variable-buffer-local 'mozc-cursor-current-input-mode)

  (defun mozc-cursor--pick-color ()
    (if mozc-cursor-current-input-mode
        mozc-cursor-color-on
      mozc-cursor-color-off))

  (defun mozc-cursor--session-execute-ad (retval)
    (prog1
        retval
      (setq-local mozc-cursor-current-input-mode
                  (when retval
                    (mozc-protobuf-get retval 'mode)))))

  (advice-add #'mozc-session-execute-command :filter-return
              #'mozc-cursor--session-execute-ad)

  (defun mozc-cursor--update-posframe (fun &rest args)
    (prog1
        (apply fun args)
      (when-let* ((buf vertico-posframe--buffer))
        (with-current-buffer buf
          (let ((color (mozc-cursor--pick-color)))
            (set-face-attribute 'cursor nil :background color))))))

  (advice-add 'vertico-posframe--show :around
              #'mozc-cursor--update-posframe)

  (defun mozc-cursor--update ()
    (let ((color (mozc-cursor--pick-color)))
      (condition-case err
          (catch 'exit
            (set-cursor-color color))
        (error (warn "Error setting cursor color")))))

  (defun mozc-cursor--on-activate ()
    (when current-input-method
      (setq-local mozc-cursor-current-input-mode 'just-activated))
    (mozc-cursor--update))

  (defun mozc-cursor--on-deactivate ()
    (setq-local mozc-cursor-current-input-mode nil)
    (mozc-cursor--update))

  (add-hook 'input-method-activate-hook #'mozc-cursor--on-activate)
  (add-hook 'input-method-deactivate-hook #'mozc-cursor--on-deactivate)
  (add-hook 'post-command-hook #'mozc-cursor--update))

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
