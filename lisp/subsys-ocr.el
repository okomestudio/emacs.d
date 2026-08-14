;;; subsys-ocr.el --- OCR  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the OCR subsystem.
;;
;;; Code:

(use-package tesseract
  ;; Tesseract OCR integration.
  :bind ("C-x C-y" . tesseract-ocr-clipboard-image)
  :custom ((doc-view-scale-internally nil)
           (tessearct/default-language "eng"))
  :config
  (defun tesseract-ocr-clipboard-image ()
    "Run OCR on an image in clipboard and paste into current position.
With the prefix argument, the command will prompt for a language."
    (interactive)
    (let* ((image-type "image/png")
           (lang (if current-prefix-arg
                     (completing-read "OCR language: "
                                      (tesseract/list-languages) nil t "")
                   tesseract/current-language))
           (cmd-paste (cond
                       ;; Wayland:
                       ((and (eq system-type 'gnu/linux)
                             (or (eq window-system 'pgtk)
                                 (getenv "WAYLAND_DISPLAY")))
                        "wl-paste -t %s")
                       ;; X:
                       ((and (eq system-type 'gnu/linux)
                             (and (eq window-system 'x)
                                  (not (getenv "WAYLAND_DISPLAY"))))
                        "xclip -selection clipboard -t %s -l 1 -o")))
           (cmd (format "%s | tesseract -l %s stdin stdout"
                        (format cmd-paste image-type) lang)))
      (insert (shell-command-to-string cmd))))

  :commands (tesseract-change-language)
  :ensure-system-package
  ("/usr/bin/tesseract" . "sudo apt install -y tesseract-ocr")
  ("/usr/share/tesseract-ocr/5/tessdata/eng.traineddata" . "sudo apt install -y tesseract-ocr-eng")
  ("/usr/share/tesseract-ocr/5/tessdata/jpn.traineddata" . "sudo apt install -y tesseract-ocr-jpn")
  ("/usr/share/tesseract-ocr/5/tessdata/jpn_vert.traineddata" . "sudo apt install -y tesseract-ocr-jpn-vert"))

(provide 'subsys-ocr)
;;; subsys-ocr.el ends here
