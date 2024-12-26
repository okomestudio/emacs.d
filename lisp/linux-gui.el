;;; linux-gui.el --- Linux GUI  -*- lexical-binding: t -*-
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

;;; Tesseract

(defcustom tesseract-ok-paste-cmd "wl-paste -t %s"
  "Image pasting shell command.")

(use-package tesseract
  :straight (:host github :repo "SebastianMeisel/tesseract.el")
  :commands (tesseract-change-language)
  :bind ("C-x C-y" . tesseract-ok-ocr-clipboard-image)
  :custom ((doc-view-scale-internally nil)
           (tessearct/default-language "eng"))

  :ensure-system-package
  ("/usr/bin/tesseract" . "sudo apt install -y tesseract-ocr")
  ("/usr/share/tesseract-ocr/5/tessdata/eng.traineddata" . "sudo apt install -y tesseract-ocr-eng")
  ("/usr/share/tesseract-ocr/5/tessdata/jpn.traineddata" . "sudo apt install -y tesseract-ocr-jpn")
  ("/usr/share/tesseract-ocr/5/tessdata/jpn_vert.traineddata" . "sudo apt install -y tesseract-ocr-jpn-vert")

  :config
  (defun tesseract-ok-ocr-clipboard-image ()
    "Run OCR on an image in clipboard and paste into current position."
    (interactive)
    (let* ((image-type "image/png")
           (cmd (format tesseract-ok-paste-cmd image-type))
           (lang tesseract/current-language))
      (insert (shell-command-to-string
               (format "%s | tesseract -l %s stdin stdout" cmd lang))))))

(use-package tesseract
  :if (and (eq system-type 'gnu/linux) (memq window-system '(pgtk)))
  :straight nil
  :config (setopt tesseract-ok-paste-cmd "wl-paste -t %s"))

(use-package tesseract
  :if (and (eq system-type 'gnu/linux) (memq window-system '(x)))
  :straight nil
  :config (setopt tesseract-ok-paste-cmd "xclip -selection clipboard -t %s -l 1 -o"))

(provide 'linux-gui)
;;; linux-gui.el ends here
