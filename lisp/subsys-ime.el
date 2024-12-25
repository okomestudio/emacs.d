;;; subsys-ime.el --- IME Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up IME subsystem.
;;
;;; Code:

(use-package mozc
  :custom ((default-input-method "japanese-mozc")
           (mozc-candidate-style 'echo-area)) ; echo-area/overlay/posframe
  :commands (toggle-input-method)
  :ensure-system-package
  ("/usr/bin/mozc_emacs_helper" . "sudo apt install -y emacs-mozc-bin"))

(use-package mozc-posframe
  :disabled  ; since using posframe with mozc cause frequent crash
  :straight (mozc-posframe :host github :repo "derui/mozc-posframe")
  ;; :config (mozc-posframe-register)
  )

(provide 'subsys-ime)
;;; subsys-ime.el ends here
