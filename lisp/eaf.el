;;; eaf.el --- Emacs Application Framework  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Emacs Application Framework.
;;
;;; Code:

(use-package eaf
  :straight (eaf
             :type git
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

;;; eaf.el ends here
