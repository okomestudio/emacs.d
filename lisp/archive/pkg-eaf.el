;;; pkg-eaf.el --- Emacs Application Framework Setup  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Emacs Application Framework.
;;
;;; Code:

(use-package eaf
  :config
  (require 'eaf-browser)
  (require 'eaf-pdf-viewer))

(provide 'pkg-eaf)
;;; pkg-eaf.el ends here
