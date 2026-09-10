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
           (mozc-leim-title "🇯🇵"))
  :init
  ;; Setting this to non-nil uses the IME on OS:
  (setq pgtk-use-im-context-on-new-connection nil)

  (defun mozc-candidate-overlay-theme (theme)
    "Style Mozc candidate overlays for THEME."
    (with-eval-after-load 'mozc
      (let ((height (face-attribute 'default :height)))
        (let ((faces (append '(mozc-cand-overlay-even-face
                               mozc-cand-overlay-odd-face
                               mozc-cand-overlay-footer-face)
                             (when (facep 'mozc-cand-overlay-description-face)
                               '(mozc-cand-overlay-description-face))))
              (parent (if (featurep 'corfu) 'corfu-default 'fixed-pitch)))
          (dolist (face faces)
            (set-face-attribute face nil
                                :foreground (face-attribute parent :foreground)
                                :background (face-attribute parent :background)
                                :inherit parent :weight 'medium :height height)))
        (let ((faces '(mozc-cand-overlay-focused-face))
              (parent (if (featurep 'corfu) 'corfu-current 'fixed-pitch)))
          (dolist (face faces)
            (set-face-attribute face nil
                                :foreground (face-attribute parent :foreground)
                                :background (face-attribute parent :background)
                                :inherit parent :weight 'medium :height height))))))

  :commands (toggle-input-method)
  :hook ((enable-theme-functions . mozc-candidate-overlay-theme)))

;;; Candidate Styling
;;
;; NOTE(2025-03-12): `posframe' would be best, but it frequently crashed
;; with Cairo in a hard-to-debug way. `overlay' is sluggish. `echo-area'
;; is sufficient and stable, but far from the point of input. `popup'
;; seems to be the best compromise?

(use-package mozc
  :if (and (eq system-type 'gnu/linux) (memq window-system '(pgtk)))
  :custom (mozc-candidate-style 'overlay))

(use-package mozc-popup
  :disabled
  :after mozc
  :hook ((on-first-input . (lambda () (require 'mozc-popup)))))

(use-package mozc-cand-posframe
  ;; For `posframe', this package may be the simplest option.
  :disabled
  :if (and (eq system-type 'gnu/linux) (not (memq window-system '(pgtk))))
  :after mozc
  :init (require 'mozc-cand-posframe))

(use-package mozc-posframe
  ;; This might work, but not out of box.
  :if (and (eq system-type 'gnu/linux) (not (memq window-system '(pgtk))))
  :custom (mozc-candidate-style 'posframe)
  :hook ((on-first-input . mozc-posframe-initialize)))

;;; Migemo

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
