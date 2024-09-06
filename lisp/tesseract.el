;;; tesseract.el --- Tesseract  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Tesseract configuration.
;;
;;; Code:

(defcustom tesseract-ok-paste-cmd "wl-paste -t %s"
  "Image pasting shell command.")

(use-package tesseract
  :if (eq system-type 'gnu/linux)
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

;;; tesseract.el ends here
