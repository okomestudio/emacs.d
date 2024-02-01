;;; 80-emms.el --- EMMS  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Music player for Emacs.
;;
;;; Code:

(use-package emms
  :defer t

  :custom
  (emms-player-list '(emms-player-mpv))
  (emms-info-functions '(emms-info-native))
  (emms-source-file-default-directory `(,(expand-file-name ".cache/emms"
                                                           user-emacs-directory)))

  :ensure-system-package
  (mpv . "sudo apt install mpv")

  :config
  (emms-all))

;;; 80-emms.el ends here
