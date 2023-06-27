;;; init-wayland.el --- Wayland-specific setup  -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; This package performs Wayland-specific setup.
;;;
;;; Code:


(use-package simple
  :straight nil

  :custom
  (interprogram-cut-function 'init-wayland--wl-copy)
  (interprogram-paste-function 'init-wayland--wl-paste)

  :ensure-system-package
  ("/usr/bin/wl-copy" . "sudo apt install -y wl-clipboard")

  :preface
  ;; Make clipboard work. Without this, copy and paste across multiple apps may
  ;; not work properly if they are in different display.
  ;;
  ;; See https://www.emacswiki.org/emacs/CopyAndPaste#h5o-4.
  (setq init-wayland--wl-copy-process nil)

  (defun init-wayland--wl-copy (text)
    (setq init-wayland--wl-copy-process (make-process :name "wl-copy"
                                          :buffer nil
                                          :command '("wl-copy" "-f" "-n")
                                          :connection-type 'pipe))
    (process-send-string init-wayland--wl-copy-process text)
    (process-send-eof init-wayland--wl-copy-process))

  (defun init-wayland--wl-paste ()
    (if (and init-wayland--wl-copy-process (process-live-p init-wayland--wl-copy-process))
        nil                 ; should return nil if we're the current paste owner
      (shell-command-to-string "wl-paste -n | tr -d \r"))))


(provide 'init-wayland)
;;; init-wayland.el ends here
