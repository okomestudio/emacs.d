;;; 02-consult.el --- Consult  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Consulting completing-read.
;;
;;; Code:

(use-package consult
  :straight
  (:pre-build
   ;; build info manual, which appears missing by default:
   (("emacs" "-Q" "-batch" "-L" "./" "--visit" "README.org" "--funcall" "org-texinfo-export-to-texinfo")
    ("makeinfo" "consult.texi" "-o" "consult.info")
    ("install-info" "consult.info" "dir")))

  :bind
  (([remap electric-apropos] . consult-apropos)
   ;; ([remap apropos] . consult-apropos)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer] . consult-buffer)

   :prefix "M-g"
   :prefix-map consult-prefix-map
   ("F" . consult-locate)
   ("G" . consult-git-grep)
   ("I" . consult-imenu-multi)
   ("L" . consult-line-multi)
   ("M" . consult-global-mark)
   ("M-g" . consult-goto-line)
   ("b" . consult-bookmark)
   ("f" . consult-find)
   ("g" . consult-ripgrep)
   ("i" . consult-imenu)
   ("l" . consult-line)
   ("m" . consult-mark)
   ("o" . consult-outline))

  :custom
  (consult-ripgrep-args (concat "rg"
                                " --null"
                                " --line-buffered"
                                " --color=never"
                                " --max-columns=1000"
                                " --path-separator /"
                                " --smart-case"
                                " --no-heading"
                                " --with-filename"
                                " --line-number"
                                " --search-zip"
                                " --follow"))

  :ensure-system-package
  (locate . "sudo apt install -y locate")

  :config
  (consult-customize consult-theme
                     :preview-key '(:debounce 0.2 any)))


(use-package consult-company
  :disabled
  :after (consult company)

  :init
  (define-key company-mode-map [remap completion-at-point] #'consult-company))


(use-package consult-flycheck
  :after (consult flycheck)

  :bind
  ("M-g c" . consult-flycheck))


(use-package consult-flyspell
  :after (consult flyspell)

  :bind
  ("M-g s" . consult-flyspell))


(use-package consult-lsp
  :after (consult lsp))


(use-package consult-projectile
  :after (consult projectile)

  :bind
  (([remap projectile-find-dir] . consult-projectile-find-dir)
   ([remap projectile-find-file] . consult-projectile-find-file)
   ([remap projectile-switch-project] . consult-projectile-switch-project)

   :map projectile-command-map
   ("A" . consult-projectile))

  :config
  ;; advise consult-projectile-switch-project to trigger project switch hooks
  ;; These projectile hooks won't trigger unless projectile-switch-project is
  ;; used.
  (advice-add #'consult-projectile-switch-project
              :around
              (lambda (orig-func)
                (run-hooks 'projectile-before-switch-project-hook)
                (funcall orig-func)
                (run-hooks 'projectile-after-switch-project-hook))))

;;; 02-consult.el ends here
