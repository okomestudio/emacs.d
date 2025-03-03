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

(use-package mozc-posframe
  :straight (mozc-posframe :host github :repo "derui/mozc-posframe")
  :after mozc
  :hook (mozc-mode . mozc-posframe-initialize))

(provide 'subsys-ime)
;;; subsys-ime.el ends here
