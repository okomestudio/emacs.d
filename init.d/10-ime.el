;;; 10-ime.el --- IME  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure Input Method.
;;
;;; Code:

(use-package mozc
  :commands (toggle-input-method)
  :custom
  (default-input-method "japanese-mozc")
  (mozc-candidate-style 'echo-area) ;; choices are 'overlay, 'echo-area, 'posframe

  :ensure-system-package
  ("/usr/bin/mozc_emacs_helper" . "sudo apt install -y emacs-mozc-bin"))


(use-package mozc-posframe
  :disabled ;; since using posframe with mozc cause frequent crash
  :straight (mozc-posframe :host github :repo "derui/mozc-posframe")
  :config (mozc-posframe-register))

;;; 10-ime.el ends here
