;;; init-straight.el --- Straight Initialization  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Initialize `straight' for use with `use-package'.
;;
;; To upgrade a package and its dependencies:
;;
;;   M-x straight-pull-package-and-deps <package>
;;   M-x straight-freeze-version
;;
;; The frozen versions will be saved to the
;; straight/versions/default.el file. Put this file under version
;; control.
;;
;; When Straight complains that a package is missing, run
;;
;;   M-x straight-pull-recipe-repositories
;;
;; to update recipe repositories.
;;
;; The following utility functions may be useful:
;;
;;   - `straight-visit-package' to browse the package source code
;;   - `straight-visit-package-website' to visit the package website
;;
;; Depending on the `straight-check-for-modifications' setting, the
;; explicit execution of
;;
;;   - M-x straight-rebuild-package
;;
;; on package modification may be necessary.
;;
;;; Code:

;; The modification check by `straight' is minimized by
;;
;;   - removing the `find-at-startup' option
;;   - using the `watch-files' option
;;
;; For the latter, github.com/watchexec/watchexec is required.
;;
(setopt straight-check-for-modifications '(watch-files))

(setopt straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file (locate-user-emacs-file
                       "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 7))  ; straight bootstrap version
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	      (url-retrieve-synchronously
         (concat "https://raw.githubusercontent.com/"
                 "radian-software/straight.el/develop/install.el")
	       'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setopt use-package-always-defer t               ; use :demand t to override
        use-package-enable-imenu-support t
        use-package-minimum-reported-time 0.001
        message-log-max t)

(require 'use-package)
(require 'use-package-ensure-system-package)

(use-package system-packages
  :custom ((system-packages-use-sudo t)
           (system-packages-package-manager 'apt)))

;;; Override Recipes
;;
;; Custom recipes that override the default are defined here, in order to avoid
;; conflicts. This is done early in `init.el' so that we minimize the use of
;; `:straight'.

(dolist
    (recipe
     `((eblook
        :type git :host github :repo "okomestudio/eblook"

        ;; Need libeb16-dev and libz-dev.
        :pre-build (("autoreconf")
                    ("./configure"
                     ,(concat "--prefix=" (expand-file-name ".local" "~")))
                    ("make") ("make" "install")))
       (org-dividers
        :type git :host github :repo "okomestudio/org-dividers")
       (org-hide-drawers
        :type git :host github :repo "krisbalintona/org-hide-drawers"
        :branch "devel")
       (org-modern-indent
        :type git :host github :repo "jdtsmith/org-modern-indent")
       (org-ok
        :type git :host github :repo "okomestudio/org-ok"
        :files (:defaults "extensions/*"))
       (ok-plural
        :type git :host github :repo "okomestudio/ok-plural.el")
       (org-roam-cjk
        :type git :host github :repo "okomestudio/org-roam-cjk"
        :files (:defaults "extensions/*"))
       (org-roam-fz
        :type git :host github :repo "okomestudio/org-roam-fz")
       (org-roam-node-display-cache
        :type git :host github :repo "okomestudio/org-roam-node-display-cache")
       (org-roam-ok
        :type git :host github :repo "okomestudio/org-roam-ok"
        :files (:defaults "extensions/*"))
       (org-transclusion
        :type git :host github :repo "nobiot/org-transclusion"
        :pre-build (("makeinfo" "./docs/org-transclusion.texi"
                     "-o" "./docs/org-transclusion.info")
                    ("install-info"
                     "./docs/org-transclusion.info" "./docs/dir"))

        ;; NOTE: See github.com/nobiot/org-transclusion/issues/271
        :branch "feat/transient")
       ))
  (straight-override-recipe recipe))

(let ((repo (expand-file-name (straight--repos-dir "lookup"))))
  (straight-override-recipe
   `(lookup
     :type git :host github :repo "okomestudio/lookup"
     :pre-build
     (("./configure"
       ,(concat "--prefix=" (file-name-concat repo "dist"))
       ,(concat "--infodir=" (file-name-concat repo "dist" "info")))
      ("make") ("make" "install"))
     :files
     (,(file-name-concat repo "dist/share/emacs/site-lisp/lookup/*.el")
      ,(file-name-concat repo "dist/info/*")))))

;; Ensure use of builtin version for the following packages:
(dolist (pkg '(ob-C
               ob-core
               ob-dot
               ob-js
               ob-plantuml
               ob-python
               ob-shell
               ob-sql
               ob-sqlite
               ob-tangle
               org-agenda
               ox
               ox-latex
               ox-md
               project))
  (straight-override-recipe `(,pkg :type built-in)))

(provide 'init-straight)
;;; init-straight.el ends here
