;;; ime.el --- IME  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; IME configuration.
;;
;;; Code:

(use-package mozc
  :commands (toggle-input-method)
  :custom ((default-input-method "japanese-mozc")
           ;; candidate style choices: 'overlay, 'echo-area, 'posframe
           (mozc-candidate-style 'echo-area))
  :ensure-system-package
  ("/usr/bin/mozc_emacs_helper" . "sudo apt install -y emacs-mozc-bin"))

(use-package mozc-posframe
  :disabled  ; since using posframe with mozc cause frequent crash
  :straight (mozc-posframe :host github :repo "derui/mozc-posframe")
  :config (mozc-posframe-register))

;;; ime.el ends here
