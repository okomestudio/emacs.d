;;; 10-eaf.el --- Emacs Application Framework  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eaf
  :disabled
  :straight
  '(eaf :type git
        :host github
        :repo "emacs-eaf/emacs-application-framework"
        :files ("*.el" "*.py" "core" "app" "*.json")
        :includes (eaf-browser eaf-pdf-viewer)
        :pre-build (("python" "install-eaf.py" "--install" "browser" "pdf-viewer" "--ignore-sys-deps")))

  :config
  (require 'eaf-browser)
  (require 'eaf-pdf-viewer))

;;; 10-eaf.el ends here
