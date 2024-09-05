;;; gui-linux.el --- Linux GUI  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Linux GUI configuration.
;;
;;; Code:

(use-package emacs ;; for X
  :if (and (eq system-type 'gnu/linux) (memq window-system '(x)))
  :straight nil
  :custom ((save-interprogram-paste-before-kill t)
           (select-enable-clipboard t)
           (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))))


(use-package emacs ;; for Wayland
  :if (and (eq system-type 'gnu/linux) (memq window-system '(pgtk)))
  :straight nil
  :custom ((interprogram-cut-function #'gui-linux-copy)
           (interprogram-paste-function #'gui-linux-paste)
           (save-interprogram-paste-before-kill t)
           (select-enable-clipboard t)
           (x-select-request-type nil))

  :ensure-system-package
  ("/usr/bin/wl-copy" . "sudo apt install -y wl-clipboard")

  :preface
  ;; Make clipboard work. Without this, copy and paste across multiple
  ;; apps may not work properly if they are in different display.
  ;;
  ;; See https://www.emacswiki.org/emacs/CopyAndPaste#h5o-4.
  ;;
  ;; IMPORTANT: The very first use of wl-paste will lead to yanking to
  ;; get stuck, due to no content. To avoid this issue, run "wl-copy
  ;; foo" in shell before starting Emacs.
  ;;
  (defvar gui-linux-copy-process nil
    "Keep an instance of wl-copy process.")

  (defun gui-linux-copy (text)
    (setq gui-linux-copy-process (make-process :name "wl-copy"
                                               :buffer nil
                                               :command '("wl-copy" "-f" "-n")
                                               :connection-type 'pipe))
    (process-send-string gui-linux-copy-process text)
    (process-send-eof gui-linux-copy-process))

  (defun gui-linux-paste ()
    (if (and gui-linux-copy-process
             (process-live-p gui-linux-copy-process))
        nil  ; should return nil if we're the current paste owner
      (shell-command-to-string "wl-paste -n | tr -d \r"))))

;;; gui-linux.el ends here
