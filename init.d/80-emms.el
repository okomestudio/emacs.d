;;; 80-emms.el --- EMMS  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Music player for Emacs.
;;
;;; Code:

(use-package emms
  :custom
  (emms-player-list '(emms-player-mpv))
  (emms-info-functions '(emms-info-native))

  :ensure-system-package
  (mpv . "sudo apt install mpv")

  :config
  (emms-all))

;;; 80-emms.el ends here
