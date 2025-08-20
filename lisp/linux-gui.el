;;; linux-gui.el --- GUI in Linux  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure GUI in Linux.
;;
;;; Code:

(use-package emacs            ; for X (including athena build)
  :if (and (eq system-type 'gnu/linux) (memq window-system '(x)))
  :custom ((save-interprogram-paste-before-kill t)
           (select-enable-clipboard t)
           (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))))


(use-package emacs            ; for Wayland
  :if (and (eq system-type 'gnu/linux) (memq window-system '(pgtk)))
  :custom ((interprogram-cut-function #'linux-gui-copy)
           (interprogram-paste-function #'linux-gui-paste)
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
  (defvar linux-gui-copy-process nil
    "Keep an instance of wl-copy process.")

  (defun linux-gui-copy (text)
    (setq linux-gui-copy-process (make-process :name "wl-copy"
                                               :buffer nil
                                               :command '("wl-copy" "-f" "-n")
                                               :connection-type 'pipe))
    (process-send-string linux-gui-copy-process text)
    (process-send-eof linux-gui-copy-process))

  (defun linux-gui-paste ()
    (if (and linux-gui-copy-process
             (process-live-p linux-gui-copy-process))
        nil  ; should return nil if we're the current paste owner
      (shell-command-to-string "wl-paste -n | tr -d \r"))))

(provide 'linux-gui)
;;; linux-gui.el ends here
