;;; themes-ok-org.el --- OK Org Themes  -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(require 'ok)

(use-package org-modern)
(use-package org-modern-indent)

(use-package ok-org-themes
  :straight (ok-org-themes
             :type git :host github :repo "okomestudio/ok-org-themes.el")
  :config
  (defun ok-org-modern-theme--prepare-fonts (theme)
    "Prepare fonts and fontsets used in the `ok-org-modern' theme."
    (when (eq theme 'ok-org-modern)
      (let* ((fontsets '(( :fontset "fontset-ok org fixed pitch"
                           :font-family "HackGen35 Console NF")
                         ( :fontset "fontset-ok org variable pitch"
                           :font-family "EB Garamond"
                           :subsets ((ja . "Noto Serif CJK JP Medium")))
                         ( :fontset "fontset-ok org outline"
                           :font-family "URW Classico"
                           :subsets ((ja . "UmePlus P Gothic"))
                           :char-specs ((?― . "EB Garamond")) ))))
        (dolist (fontset fontsets)
          (ok-fontset-create (plist-get fontset :fontset)
                             (plist-get fontset :font-family)
                             :subsets (plist-get fontset :subsets)
                             :char-specs (plist-get fontset :char-specs)))

        ;; Customize faces
        (set-face-attribute 'ok-org-modern-fixed-pitch nil
                            :family "HackGen35 Console NF"
                            :fontset "fontset-fixed pitch")
        (set-face-attribute 'ok-org-modern-variable-pitch nil
                            :family "EB Garamond"
                            :fontset "fontset-variable pitch")
        (set-face-attribute 'ok-org-modern-outline nil
                            :family "URW Classico"
                            :fontset "fontset-ok org outline"))))

  (add-hook 'before-enable-theme-functions
            #'ok-org-modern-theme--prepare-fonts -98)

  (load-theme 'ok-org-modern t t))

(provide 'themes-ok-org)
;;; themes-ok-org.el ends here
