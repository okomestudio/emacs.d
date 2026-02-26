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
;; For the latter option, github.com/watchexec/watchexec is required. For Debian
;; Trixie, watchexec-2.3.2-x86_64-unknown-linux-gnu.deb was installed
;; (2025-08-17).
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
     `(( aio git github "skeeto/emacs-aio"
         ;; See https://github.com/skeeto/emacs-aio/issues/31.
         :fork (:host github :repo "kiennq/emacs-aio") )
       ( anki-editor git github "anki-editor/anki-editor"
         :fork ( :host github :repo "okomestudio/anki-editor"
                 :branch "enable-file-based-note") )
       ( atomic-chrome git github "KarimAziev/atomic-chrome" :flavor nil )
       ( bibtex-completion-ok git github "okomestudio/bibtex-completion-ok" )
       ( blamer git github "artawower/blamer.el" )
       ( boem-weather git gitlab "boskoivanisevic/boem-weather" )
       ( cal-japan git github "kawabata/cal-japan" )
       ( company-tern file nil nil :fetcher url
         :url ,(file-name-concat
                "https://gist.githubusercontent.com"
                "okomestudio/de8c59960ce8f195ee0224de5db5a168/raw"
                "1193992ffeeca8193ebf459b377c27f628ac3246/company-tern.el") )
       ( consult git github "minad/consult"
         :pre-build
         ;; Build info manual, which is missing by default:
         (("emacs" "-Q" "-batch" "-L" "./"
           "--visit" "README.org"
           "--funcall" "org-texinfo-export-to-texinfo")
          ("makeinfo" "consult.texi" "-o" "consult.info")
          ;; ("install-info" "consult.info" "dir")
          ) )
       ( democratize git sourcehut "flandrew/democratize" )
       ( do-this-now git github "okomestudio/do-this-now.el" )
       ( eaf git github "emacs-eaf/emacs-application-framework"
         :files ("*.el" "*.py" "core" "app" "*.json")
         :includes (eaf-browser eaf-pdf-viewer)
         :pre-build
         (("python" "install-eaf.py"
           "--install" "browser" "pdf-viewer" "--ignore-sys-deps")) )
       ( eat git codeberg "akib/emacs-eat"
         :files ("*.el" ("term" "term/*.el") "*.texi"
                 "*.ti" ("terminfo/e" "terminfo/e/*")
                 ("terminfo/65" "terminfo/65/*")
                 ("integration" "integration/*")
                 (:exclude ".dir-locals.el" "*-tests.el")) )
       ( eaw git github "hamano/locale-eaw" )
       ( eblook git github "okomestudio/eblook"
         ;; Need libeb16-dev and libz-dev.
         :pre-build (("autoreconf")
                     ("./configure"
                      ,(concat "--prefix=" (expand-file-name ".local" "~")))
                     ("make") ("make" "install")) )
       ( elisp-for-python git github "kickingvegas/elisp-for-python" )
       ( emacs-lisp-elements git github "protesilaos/emacs-lisp-elements" )
       ( flycheck git github "flycheck/flycheck" )
       ( flycheck-aspell-org git github "okomestudio/flycheck-aspell-org.el" )
       ( flyover git github "konrad1977/flyover" )
       ( greppu git github "okomestudio/greppu.el" )
       ( hatsuon git github "okomestudio/hatsuon.el"
         :files (:defaults "extensions/*") )
       ( help-shortdoc-example git github "buzztaiki/help-shortdoc-example.el" )
       ( indent-bars git github "jdtsmith/indent-bars" )
       ( kaomel git github "gicrisf/kaomel" :files ("*.el"))
       ( keychain-environment git github "tarsius/keychain-environment" )
       ( lsp-booster git github "okomestudio/lsp-booster.el"
         :post-build (("make")) )
       ( lsp-bridge git github "manateelazycat/lsp-bridge"
         :files (:defaults "*.el"
                           "*.py"
                           "acm"
                           "core"
                           "langserver"
                           "multiserver"
                           "resources")
         :build (:not compile) )
       ( man-index git codeberg "imarko/man-index" )
       ( mozc git github "google/mozc"
         ;; NOTE(2025-03-04): Pin to a commit to avoid divergence of submodules
         ;; refs to cause dirty repo.
         :commit "14afac9728dd3f04e3d73633f4fa925d38589368" )
       ( mozc-isearch git github "iRi-E/mozc-el-extensions" )
       ( mozc-cand-posframe git github "akirak/mozc-posframe" )
       ( mozc-popup git github "d5884/mozc-popup"
         :fork (:branch "reduce-refresh") )
       ( mozc-posframe git github "derui/mozc-posframe"
         :fork (:branch "ok") )
       ( mulex git github "okomestudio/mulex" )
       ( ok git github "okomestudio/ok.el" )
       ( ok-plural git github "okomestudio/ok-plural.el" )
       ( org-block-capf git github "xenodium/org-block-capf" )
       ( org-dividers git github "okomestudio/org-dividers" )
       ( org-excalidraw git github "4honor/org-excalidraw" )
       ( org-hide-drawers git github "krisbalintona/org-hide-drawers"
         :branch "devel" )
       ( org-id-ext git github "okomestudio/org-id-ext" )
       ( org-modern-indent git github "jdtsmith/org-modern-indent" )
       ( org-ok git github "okomestudio/org-ok"
         :files (:defaults "extensions/*") )
       ( org-roam git github "org-roam/org-roam"
         :files (:defaults "extensions/*" "org-roam-pkg.el")
         :fork t )
       ( org-roam-cjk git github "okomestudio/org-roam-cjk"
         :files (:defaults "extensions/*") )
       ( org-roam-fz git github "okomestudio/org-roam-fz" )
       ( org-roam-fztl git github "okomestudio/org-roam-fztl" )
       ( org-roam-node-display-cache
         git github "okomestudio/org-roam-node-display-cache" )
       ( org-roam-ok git github "okomestudio/org-roam-ok"
         :files (:defaults "extensions/*") )
       ( org-transclusion git github "nobiot/org-transclusion"
         :pre-build (("sh" "-c" "cd ./docs && make org-transclusion.texi")
                     ("makeinfo" "./docs/org-transclusion.texi"
                      "-o" "./docs/org-transclusion.info")
                     ("install-info"
                      "./docs/org-transclusion.info" "./docs/dir")))
       ( powerthesaurus git github "doomelpa/powerthesaurus" )
       ( py-isort git github "paetzke/py-isort.el"
         ;; For https://github.com/paetzke/py-isort.el/pull/21
         :fork ( :host github
                 :repo "okomestudio/py-isort.el"
                 :branch "ts/provide-default-settings-path" ) )
       ( pydoc git github "statmobile/pydoc" )
       ( pydoc-plugins git github "okomestudio/emacs-pydoc-plugins"
         :files (:defaults "extensions/*") )
       ( pyenv git github "aiguofer/pyenv.el" )
       ( pyenv-mode git github "pythonic-emacs/pyenv-mode")
       ( pyimport git github "Wilfred/pyimport"
         :branch "master"
         :fork ( :host github
                 :repo "okomestudio/pyimport"
                 :branch "venv-support" )
         :files ("*.el" ("bin/make-imports.py" . "bin/make-imports.py")) )
       ( pymacs git github "Pymacs2/Pymacs"
         :post-build            ; see what install-pymacs.sh does
         (("pip" "install" "-U" "pyopenssl")
          ("pip" "install" "-e"
           ,(expand-file-name (straight--repos-dir "Pymacs")))) )
       ( python-sql-mode git github "okomestudio/python-sql-mode.el" )
       ( rainbow-csv git github "emacs-vs/rainbow-csv" )
       ( ropemacs git github "python-rope/ropemacs"
         :files nil
         :post-build
         (("pip" "install" "-U" "rope")
          ("pip" "install" "-e"
           ,(expand-file-name (straight--repos-dir "ropemacs")))) )
       ( scrollpanel git nil
         "https://seed.pipapo.org/z2hyKqTqB77vt7UqsTgrqHkC83j8h.git"
         :local-repo "scrollpanel" )
       ( sql-upcase file nil nil :fetcher url
         :url ,(file-name-concat
                "https://raw.githubusercontent.com"
                "emacsmirror/emacswiki.org/master/sql-upcase.el") )
       ( tesseract git github "SebastianMeisel/tesseract.el" )
       ( time-zones git github "xenodium/time-zones")
       ( tooltipper git github "okomestudio/tooltipper.el" )
       ( treesit-fold git github "emacs-tree-sitter/treesit-fold" )
       ( typo git github "jorgenschaefer/typoel" )
       ( ultra-scroll git github "jdtsmith/ultra-scroll" )
       ( urbandict.el git github "okomestudio/urbandict.el" )
       ( webkit git github "akirakyle/emacs-webkit"
         :branch "main"
         :files (:defaults "*.js" "*.css" "*.so")
         :pre-build ("make") )
       ( worg git sourcehut "bzg/worg" )
       ( xht git sourcehut "flandrew/xht" )))
  (let ((feature (nth 0 recipe))
        (type (nth 1 recipe))
        (host (nth 2 recipe))
        (repo (nth 3 recipe)))
    (straight-override-recipe
     `(,feature :type ,type :host ,host :repo ,repo ,@(seq-subseq recipe 4)))))

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
