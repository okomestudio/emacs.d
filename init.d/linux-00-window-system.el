;;; linux-00-window-system.el --- Linux window system  -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; This package performs Linux Window system-specific config.
;;;
;;; Code:

(use-package emacs
  :if (and (eq system-type 'gnu/linux) (memq window-system '(x)))
  :straight nil

  :custom
  (save-interprogram-paste-before-kill t)
  (select-enable-clipboard t)
  (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))


(use-package emacs
  :if (and (eq system-type 'gnu/linux) (memq window-system '(pgtk)))
  :straight nil

  :custom
  (interprogram-cut-function #'ok--wl-copy)
  (interprogram-paste-function #'ok--wl-paste)
  (save-interprogram-paste-before-kill t)
  (select-enable-clipboard t)
  (x-select-request-type nil)

  :ensure-system-package
  ("/usr/bin/wl-copy" . "sudo apt install -y wl-clipboard")

  :preface
  ;; Make clipboard work. Without this, copy and paste across multiple apps may
  ;; not work properly if they are in different display.
  ;;
  ;; See https://www.emacswiki.org/emacs/CopyAndPaste#h5o-4.
  ;;
  ;; IMPORTANT: The very first use of wl-paste will lead to yanking to get
  ;; stuck, due to no content. To avoid this issue, run "wl-copy foo" in shell
  ;; before starting Emacs.
  ;;
  (defvar ok--wl-copy-process nil
    "Keep an instance of wl-copy process.")

  (defun ok--wl-copy (text)
    (setq ok--wl-copy-process (make-process :name "wl-copy"
                                          :buffer nil
                                          :command '("wl-copy" "-f" "-n")
                                          :connection-type 'pipe))
    (process-send-string ok--wl-copy-process text)
    (process-send-eof ok--wl-copy-process))

  (defun ok--wl-paste ()
    (if (and ok--wl-copy-process
             (process-live-p ok--wl-copy-process))
        nil ;; should return nil if we're the current paste owner
      (shell-command-to-string "wl-paste -n | tr -d \r"))))

;; Local Variables:
;; nameless-aliases: (("" . "ok"))
;; End:
;;; linux-00-window-system.el ends here
