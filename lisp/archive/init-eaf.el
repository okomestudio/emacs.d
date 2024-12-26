;;; init-eaf.el --- Emacs Application Framework Setup  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Emacs Application Framework.
;;
;;; Code:

(use-package eaf
  :straight (eaf :type git
                 :host github
                 :repo "emacs-eaf/emacs-application-framework"
                 :files ("*.el" "*.py" "core" "app" "*.json")
                 :includes (eaf-browser eaf-pdf-viewer)
                 :pre-build (("python" "install-eaf.py"
                              "--install" "browser" "pdf-viewer"
                              "--ignore-sys-deps")))
  :config
  (require 'eaf-browser)
  (require 'eaf-pdf-viewer))

(provide 'init-eaf)
;;; init-eaf.el ends here
