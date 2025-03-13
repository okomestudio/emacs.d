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
  :custom `((default-input-method "japanese-mozc")

            ;; NOTE(2025-03-03): `posframe' would be best, but it
            ;; frequently crashed with cairo. `overlay' runs sluggish.
            ;; `echo-area' is sufficient and stable.
            (mozc-candidate-style ,(if (display-graphic-p)
                                       'posframe
                                     'echo-area))
            (mozc-leim-title "🇯🇵"))
  :commands (toggle-input-method))

(use-package mozc
  :if (and (eq system-type 'gnu/linux) (memq window-system '(pgtk)))
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

(use-package mozc-cand-posframe
  :straight (mozc-cand-posframe :host github :repo "akirak/mozc-posframe")
  :if (display-graphic-p)
  :after mozc
  :init (require 'mozc-cand-posframe))

(use-package mozc-posframe
  :disabled
  :straight (mozc-posframe :host github :repo "derui/mozc-posframe")
  :after mozc
  :hook ((mozc-mode . mozc-posframe-initialize)
         (enable-theme-functions . mozc-posframe-ok--theme))
  :config
  (defun mozc-posframe-ok--theme (theme)
    (when (and (boundp 'corfu-mode) corfu-mode)
      (let* ((face-base 'corfu-default)
             (face-current 'corfu-current)
             (face-annotations 'corfu-annotations)
             (foreground (face-attribute face-base :foreground))
             (background (face-attribute face-base :background)))
        (set-face-attribute 'mozc-cand-overlay-even-face nil
                            :foreground foreground
                            :background background
                            :inherit face-base)
        (set-face-attribute 'mozc-cand-overlay-odd-face nil
                            :foreground foreground
                            :background background
                            :inherit face-base)
        (set-face-attribute 'mozc-cand-overlay-footer-face nil
                            :foreground foreground
                            :background background
                            :inherit face-base)
        (set-face-attribute 'mozc-cand-overlay-focused-face nil
                            :foreground (face-attribute face-current :foreground)
                            :background (face-attribute face-current :background)
                            :inherit face-current)
        (with-eval-after-load 'mozc-posframe
          (set-face-attribute 'mozc-cand-overlay-description-face nil
                              :foreground (face-attribute face-annotations :foreground)
                              :background (face-attribute face-annotations :background)
                              :inherit face-annotations))))))

(provide 'subsys-ime)
;;; subsys-ime.el ends here
