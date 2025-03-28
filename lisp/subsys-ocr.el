;;; subsys-ocr.el --- subsys-ocr  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; The OCR subsystem.
;;
;;; Code:

(defcustom tesseract-ok-paste-cmd "wl-paste -t %s"
  "Image pasting shell command."
  :type 'string
  :group 'tessearact)

(use-package tesseract
  :straight (tesseract :host github :repo "SebastianMeisel/tesseract.el")
  :bind ("C-x C-y" . tesseract-ok-ocr-clipboard-image)
  :custom ((doc-view-scale-internally nil)
           (tessearct/default-language "eng"))
  :commands (tesseract-change-language)
  :ensure-system-package
  ("/usr/bin/tesseract" . "sudo apt install -y tesseract-ocr")
  ("/usr/share/tesseract-ocr/5/tessdata/eng.traineddata"
   . "sudo apt install -y tesseract-ocr-eng")
  ("/usr/share/tesseract-ocr/5/tessdata/jpn.traineddata"
   . "sudo apt install -y tesseract-ocr-jpn")
  ("/usr/share/tesseract-ocr/5/tessdata/jpn_vert.traineddata"
   . "sudo apt install -y tesseract-ocr-jpn-vert")

  :config
  (defun tesseract-ok-ocr-clipboard-image (&optional arg)
    "Run OCR on an image in clipboard and paste into current position.
With a prefix argument, the function prompts for the language."
    (interactive "P")
    ;; (setq tesseract-ok-paste-cmd "wl-paste -t %s")
    (let* ((image-type "image/png")
           (cmd (format tesseract-ok-paste-cmd image-type))
           (lang (pcase arg
                   ('(4)
                    (completing-read "OCR language: "
                                     (tesseract/list-languages)
                                     nil t ""))
                   (_ tesseract/current-language))))
      (insert (shell-command-to-string
               (format "%s | tesseract -l %s stdin stdout" cmd lang))))))

(use-package tesseract        ; for Wayland
  :straight nil
  :if (and (eq system-type 'gnu/linux)
           (or (eq window-system 'pgtk)
               (getenv "WAYLAND_DISPLAY")))
  :custom (tesseract-ok-paste-cmd "wl-paste -t %s"))

(use-package tesseract        ; for X
  :straight nil
  :if (and (eq system-type 'gnu/linux)
           (and (eq window-system 'x)
                (not (getenv "WAYLAND_DISPLAY"))))
  :custom (tesseract-ok-paste-cmd "xclip -selection clipboard -t %s -l 1 -o"))

(provide 'subsys-ocr)
;;; subsys-ocr.el ends here
