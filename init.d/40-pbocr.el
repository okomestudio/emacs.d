;;; 40-pbocr.el --- Pbocr  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Enable OCR directly on clipboard image.
;;
;; TODO: Package this up properly.
;;
;;; Code:

(use-package emacs
  :bind
  ("C-x C-y" . init-pbocr--pbocr)

  :ensure-system-package
  ("/usr/bin/tesseract" . "sudo apt install -y tesseract-ocr")
  ("/usr/bin/wl-copy" . "sudo apt install -y wl-clipboard")

  :init
  (defun init-pbocr--pbocr ()
    "Run OCR on the image in clipboard and paste the text."
    (interactive)
    (insert (shell-command-to-string
             (expand-file-name "bin/pbocr" user-emacs-directory)))))

;;; 40-pbocr.el ends here
