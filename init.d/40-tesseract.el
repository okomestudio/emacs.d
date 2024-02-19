;;; 40-tesseract.el --- tesseract  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure Tesseract and related utilities.
;;
;;; Code:

(use-package tesseract
  :if (eq system-type 'gnu/linux)
  :straight (:host github :repo "SebastianMeisel/tesseract.el")
  :commands (tesseract-change-language)
  :bind
  ("C-x C-y" . ok-tesseract/ocr-clipboard-image)

  :custom
  (doc-view-scale-internally nil)
  (tessearct/default-language "eng")

  :ensure-system-package
  ("/usr/bin/tesseract" . "sudo apt install -y tesseract-ocr")
  ("/usr/share/tesseract-ocr/5/tessdata/eng.traineddata" . "sudo apt install -y tesseract-ocr-eng")
  ("/usr/share/tesseract-ocr/5/tessdata/jpn.traineddata" . "sudo apt install -y tesseract-ocr-jpn")
  ("/usr/share/tesseract-ocr/5/tessdata/jpn_vert.traineddata" . "sudo apt install -y tesseract-ocr-jpn-vert")

  :config
  (defun ok-tesseract/ocr-clipboard-image ()
    "Run OCR on an image in clipboard and paste into current position."
    (interactive)
    (let* ((image-type "image/png")
           (paste-cmd (format "wl-paste -t %s" image-type))
           (paste-cmd-x (format "xclip -selection clipboard -t %s -l 1 -o" image-type))
           (lang tesseract/current-language))
      (insert (shell-command-to-string
               (format "%s | tesseract -l %s stdin stdout" paste-cmd lang))))))

;;; 40-tesseract.el ends here
