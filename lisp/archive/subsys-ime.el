;;; subsys-ime.el --- Input Method Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the input method subsystem.
;;
;;; Code:

(use-package mozc-popup
  :disabled
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
