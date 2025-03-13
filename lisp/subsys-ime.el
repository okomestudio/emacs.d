;;; subsys-ime.el --- Input Method Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the input method subsystem.
;;
;;; Code:

(use-package mozc
  :straight (mozc :host github
                  :repo "google/mozc"

                  ;; NOTE(2025-03-04): Pin to a commit to avoid
                  ;; divergence of submodules refs to cause dirty
                  ;; repo.
                  :commit "14afac9728dd3f04e3d73633f4fa925d38589368")
  :custom ((default-input-method "japanese-mozc")

           ;; NOTE(2025-03-12): `posframe' would be best, but it
           ;; frequently crashed with Cairo in a hard-to-debug way.
           ;; `overlay' runs sluggish. `echo-area' is sufficient and
           ;; stable, but far from the point of input. `popup' seems
           ;; to be the best compromise.
           (mozc-candidate-style (if (display-graphic-p) 'popup 'echo-area))

           (mozc-leim-title "🇯🇵"))
  :commands (toggle-input-method))

(use-package mozc
  ;; :if (and (eq system-type 'gnu/linux) (memq window-system '(pgtk)))
  :init
  ;; Setting this to non-nil uses the IME on OS:
  (setq pgtk-use-im-context-on-new-connection nil))

(use-package mozc-isearch
  :straight (mozc-isearch :host github :repo "iRi-E/mozc-el-extensions")
  :hook (isearch-mode . mozc-isearch-ok--fix-ace-isearch)
  :init (require 'mozc-isearch)
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
  :straight (mozc-cand-posframe :host github :repo "akirak/mozc-posframe")
  :if (display-graphic-p)
  :after mozc
  :init (require 'mozc-cand-posframe))

(use-package mozc-posframe
  ;; This might work, but not out of box.
  :disabled
  :straight (mozc-posframe :host github :repo "derui/mozc-posframe")
  :after mozc
  :hook ((mozc-mode . mozc-posframe-initialize)
         (enable-theme-functions . mozc-posframe-ok--theme))
  :config
  (defun mozc-posframe-ok--theme (theme)
    (require 'corfu)
    (let* ((face-base 'corfu-default)
           (face-current 'corfu-current)
           (face-annotations 'corfu-annotations)
           (foreground (face-attribute face-base :foreground))
           (background (face-attribute face-base :background)))
      (dolist (face '(mozc-cand-overlay-even-face
                      mozc-cand-overlay-odd-face
                      mozc-cand-overlay-description-face
                      mozc-cand-overlay-footer-face))
        (set-face-attribute face nil
                            :foreground foreground
                            :background background
                            :inherit face-base))
      (set-face-attribute 'mozc-cand-overlay-focused-face nil
                          :foreground (face-attribute face-current :foreground)
                          :background (face-attribute face-current :background)
                          :inherit face-current))))

;;; Popup

(use-package mozc-popup
  :straight (mozc-popup :host github
                        :repo "d5884/mozc-popup"
                        :fork (:branch "reduce-refresh"))
  :after mozc
  :hook ((on-first-input . (lambda () (require 'mozc-popup)))
         (enable-theme-functions . mozc-popup-ok--theme))
  :config
  (defun mozc-popup-ok--theme (theme)
    (require 'corfu)
    (let* ((face-base 'corfu-default)
           (face-current 'corfu-current)
           (face-annotations 'corfu-annotations)
           (foreground (face-attribute face-base :foreground))
           (background (face-attribute face-base :background)))
      (dolist (face '(mozc-cand-overlay-even-face
                      mozc-cand-overlay-odd-face
                      mozc-cand-overlay-description-face
                      mozc-cand-overlay-footer-face))
        (set-face-attribute face nil
                            :foreground foreground
                            :background background
                            :inherit face-base))
      (set-face-attribute 'mozc-cand-overlay-focused-face nil
                          :foreground (face-attribute face-current :foreground)
                          :background (face-attribute face-current :background)
                          :inherit face-current))))

(provide 'subsys-ime)
;;; subsys-ime.el ends here
