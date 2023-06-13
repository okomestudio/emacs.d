;;; init-ime.el --- IME  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package mozc
  :custom
  (default-input-method "japanese-mozc")
  (mozc-candidate-style 'posframe)

  :ensure-system-package ("/usr/bin/mozc_emacs_helper" . "sudo apt install -y emacs-mozc-bin mozc-server"))

(use-package mozc-posframe
  :straight (mozc-posframe :type git :host github :repo "derui/mozc-posframe")
  :config (mozc-posframe-register))

(provide 'init-ime)
;;; init-ime.el ends here
