;;; subsys-audio.el --- Audio Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the audio subsystem.
;;
;;; Code:

(use-package emms
  :custom ((emms-player-list '(emms-player-mpv))
           (emms-info-functions '(emms-info-native)))
  :ensure-system-package (mpv . "sudo apt install mpv")
  :config (emms-all))

(provide 'subsys-audio)
;;; subsys-audio.el ends here
