;;; subsys-ime.el --- IME Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up IME subsystem.
;;
;;; Code:

(use-package mozc
  :custom ((default-input-method "japanese-mozc")
           (mozc-candidate-style 'posframe)  ; echo-area/overlay/posframe
           (mozc-leim-title "🇯🇵"))
  :commands (toggle-input-method))

(use-package mozc
  :if (and (eq system-type 'gnu/linux) (memq window-system '(pgtk)))
  :init
  ;; Setting this to non-nil uses the IME on OS:
  (setq pgtk-use-im-context-on-new-connection nil))

(use-package mozc-posframe
  :straight (mozc-posframe :host github :repo "derui/mozc-posframe")
  :after mozc
  :hook (mozc-mode . mozc-posframe-initialize))

(provide 'subsys-ime)
;;; subsys-ime.el ends here
