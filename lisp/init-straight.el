;;; init-straight.el --- Straight Initialization  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Run this module to use `straight' and `use-package' for package
;; configuration.
;;
;; To update package repositories, run
;;
;;   M-x straight-pull-recipe-repositories
;;
;; To upgrade an individual package and its dependencies, run
;;
;;   M-x straight-pull-package-and-deps <package>
;;
;; In order to allow deterministic recovery of packages, create a version lock
;; file by running
;;
;;   M-x straight-freeze-version
;;
;; The version lock file will at 'straight/versions/default.el'. (Optionally put
;; this file under version control.) To recover packages using this file, run
;;
;;   M-x straight-thaw-versions
;;
;; The following utility functions may be useful:
;;
;;   - `straight-visit-package' to browse the package source code
;;   - `straight-visit-package-website' to visit the package website
;;
;; Depending on the `straight-check-for-modifications' setting, the explicit
;; execution of
;;
;;   - M-x straight-rebuild-package
;;
;; on package modification may be necessary.
;;
;;; Code:

;; `straight' check for modifications. This check is minimized by
;;
;;   - removing the `find-at-startup' option
;;   - using the `watch-files' option
;;
;; For the latter option, github.com/watchexec/watchexec is required.
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
     `((aio
        :type git :host github :repo "skeeto/emacs-aio"
        ;; See https://github.com/skeeto/emacs-aio/issues/31.
        :fork (:host github :repo "kiennq/emacs-aio"))
       (anki-editor
        :type git :host github :repo "anki-editor/anki-editor"
        :fork ( :host github :repo "okomestudio/anki-editor"
                :branch "enable-file-based-note") )
       (atomic-chrome
        :type git :host github :repo "KarimAziev/atomic-chrome"
        :flavor nil)
       (blamer
        :type git :host github :repo "artawower/blamer.el")
       (company-tern
        :type file :fetcher url
        :url ,(file-name-concat
               "https://gist.githubusercontent.com"
               "okomestudio/de8c59960ce8f195ee0224de5db5a168"
               "raw/1193992ffeeca8193ebf459b377c27f628ac3246/company-tern.el"))
       (consult
        :pre-build
        ;; build info manual, which appears missing by default:
        (("emacs" "-Q" "-batch" "-L" "./"
          "--visit" "README.org"
          "--funcall" "org-texinfo-export-to-texinfo")
         ("makeinfo" "consult.texi" "-o" "consult.info")
         ;; ("install-info" "consult.info" "dir")
         ) )
       (do-this-now
        :type git :host github :repo "okomestudio/do-this-now.el")
       (eaf
        :type git :host github :repo "emacs-eaf/emacs-application-framework"
        :files ("*.el" "*.py" "core" "app" "*.json")
        :includes (eaf-browser eaf-pdf-viewer)
        :pre-build
        (("python" "install-eaf.py"
          "--install" "browser" "pdf-viewer" "--ignore-sys-deps")))
       (eat
        :type git :host codeberg :repo "akib/emacs-eat"
        :files ("*.el" ("term" "term/*.el") "*.texi"
                "*.ti" ("terminfo/e" "terminfo/e/*")
                ("terminfo/65" "terminfo/65/*")
                ("integration" "integration/*")
                (:exclude ".dir-locals.el" "*-tests.el")))
       (eaw
        :type git :host github :repo "hamano/locale-eaw")
       (eblook
        :type git :host github :repo "okomestudio/eblook"

        ;; Need libeb16-dev and libz-dev.
        :pre-build (("autoreconf")
                    ("./configure"
                     ,(concat "--prefix=" (expand-file-name ".local" "~")))
                    ("make") ("make" "install")))
       (elisp-for-python
        :type git :host github :repo "kickingvegas/elisp-for-python")
       (emacs-lisp-elements
        :type git :host github :repo "protesilaos/emacs-lisp-elements")
       (flycheck-aspell-org
        :type git :host github :repo "okomestudio/flycheck-aspell-org.el")
       (flyover
        :type git :host github :repo "konrad1977/flyover")
       (greppu
        :type git :host github :repo "okomestudio/greppu.el")
       (hatsuon
        :type git :host github :repo "okomestudio/hatsuon.el"
        :files (:defaults "extensions/*"))
       (help-shortdoc-example
        :type git :host github :repo "buzztaiki/help-shortdoc-example.el")
       (indent-bars
        :type git :host github :repo "jdtsmith/indent-bars")
       (keychain-environment
        :type git :host github :repo "tarsius/keychain-environment")
       (lsp-booster
        :type git :host github :repo "okomestudio/lsp-booster.el"
        :post-build (("make")))
       (lsp-bridge
        :type git :host github :repo "manateelazycat/lsp-bridge"
        :files (:defaults "*.el"
                          "*.py"
                          "acm"
                          "core"
                          "langserver"
                          "multiserver"
                          "resources")
        :build (:not compile))
       (man-index
        :type git :host codeberg :repo "imarko/man-index")
       (mozc
        :type git :host github :repo "google/mozc"

        ;; NOTE(2025-03-04): Pin to a commit to avoid
        ;; divergence of submodules refs to cause dirty
        ;; repo.
        :commit "14afac9728dd3f04e3d73633f4fa925d38589368")
       (mozc-isearch
        :type git :host github :repo "iRi-E/mozc-el-extensions")
       (mozc-cand-posframe
        :type git :host github :repo "akirak/mozc-posframe")
       (mozc-popup
        :type git :host github :repo "d5884/mozc-popup"
        :fork (:branch "reduce-refresh"))
       (mozc-posframe
        :type git :host github :repo "derui/mozc-posframe"
        :fork (:branch "ok"))
       (ok
        :type git :host github :repo "okomestudio/ok.el")
       (ok-plural
        :type git :host github :repo "okomestudio/ok-plural.el")
       (org-block-capf
        :type git :host github :repo "xenodium/org-block-capf")
       (org-dividers
        :type git :host github :repo "okomestudio/org-dividers")
       (org-excalidraw
        :type git :host github :repo "4honor/org-excalidraw")
       (org-hide-drawers
        :type git :host github :repo "krisbalintona/org-hide-drawers"
        :branch "devel")
       (org-modern-indent
        :type git :host github :repo "jdtsmith/org-modern-indent")
       (org-ok
        :type git :host github :repo "okomestudio/org-ok"
        :files (:defaults "extensions/*"))
       (org-roam
        :type git :host github :repo "org-roam/org-roam"
        :files (:defaults "extensions/*" "org-roam-pkg.el")
        :fork t)
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
       (powerthesaurus
        :type git :host github :repo "doomelpa/powerthesaurus")
       (py-isort
        :type git :host github :repo "paetzke/py-isort.el"
        ;; For https://github.com/paetzke/py-isort.el/pull/21
        :fork ( :host github
                :repo "okomestudio/py-isort.el"
                :branch "ts/provide-default-settings-path" ))
       (pydoc
        :type git :host github :repo "statmobile/pydoc")
       (pydoc-treesit
        :type git :host github :repo "okomestudio/pydoc-treesit"
        :files (:defaults "extensions/*"))
       (pyenv
        :type git :host github :repo "aiguofer/pyenv.el")
       (pyenv-mode
        :type git :host github :repo "pythonic-emacs/pyenv-mode")
       (pyimport
        :type git :host github :repo "Wilfred/pyimport"
        :branch "master"
        :fork ( :host github
                :repo "okomestudio/pyimport"
                :branch "venv-support" )
        :files ("*.el" ("bin/make-imports.py" . "bin/make-imports.py")))
       (pymacs
        :type git :host github :repo "Pymacs2/Pymacs"
        :post-build            ; see what install-pymacs.sh does:
        (("pip" "install" "-U" "pyopenssl")
         ("pip" "install" "-e"
          ,(expand-file-name (straight--repos-dir "Pymacs")))))
       (python-sql-mode
        :type git :host github :repo "okomestudio/python-sql-mode.el")
       (rainbow-csv
        :type git :host github :repo "emacs-vs/rainbow-csv")
       (ropemacs
        :type git :host github :repo "python-rope/ropemacs"
        :files nil
        :post-build
        (("pip" "install" "-U" "rope")
         ("pip" "install" "-e"
          ,(expand-file-name (straight--repos-dir "ropemacs")))))
       (sql-upcase
        :type file :fetcher url
        :url ,(file-name-concat
               "https://raw.githubusercontent.com"
               "emacsmirror/emacswiki.org/master/sql-upcase.el"))
       (tesseract
        :type git :host github :repo "SebastianMeisel/tesseract.el")
       (tooltipper
        :type git :host github :repo "okomestudio/tooltipper.el")
       (treesit-fold
        :type git :host github :repo "emacs-tree-sitter/treesit-fold")
       (typo
        :type git :host github :repo "jorgenschaefer/typoel")
       (ultra-scroll
        :type git :host github :repo "jdtsmith/ultra-scroll")
       (urbandict.el
        :type git :host github :repo "okomestudio/urbandict.el")
       (webkit
        :type git :host github :repo "akirakyle/emacs-webkit"
        :branch "main"
        :files (:defaults "*.js" "*.css" "*.so")
        :pre-build ("make"))))
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
(dolist
    (pkg
     '( ansi-color apropos auth-source autorevert
        browse-url
        cc-mode conf-mode css-mode
        desktop
        eglot elec-pair elisp-mode elp
        faces flyspell frame
        gnus gnus-art gnus-group gnus-sum
        help hideshow hippie-exp hl-line
        ibuffer image-mode indent info ispell
        js json-ts-mode
        lisp
        message mhtml-mode minibuffer mwheel
        nnfolder
        ob-C ob-core ob-dot ob-js ob-plantuml ob-python ob-shell ob-sql
        ob-sqlite ob-tangle
        org-agenda
        ox ox-latex ox-md
        pixel-scroll prog-mode project python
        recentf repeat
        save-place savehist sh-script
        toml-ts-mode tramp treesit
        which-key whitespace winner
        yaml-ts-mode ))
  (straight-override-recipe `(,pkg :type built-in)))

(provide 'init-straight)
;;; init-straight.el ends here
