;;; subsys-consult.el --- Consult Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Consult subsystem.
;;
;; Consulting `completing-read' for search and navigation.
;;
;;; Code:

(use-package consult
  :straight (:pre-build
             ;; build info manual, which appears missing by default:
             (("emacs" "-Q" "-batch" "-L" "./"
               "--visit" "README.org"
               "--funcall" "org-texinfo-export-to-texinfo")
              ("makeinfo" "consult.texi" "-o" "consult.info")
              ;; ("install-info" "consult.info" "dir")
              ))
  :bind (([remap electric-apropos] . consult-apropos)
         ;; ([remap apropos] . consult-apropos)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer] . consult-buffer)

         :prefix-map consult-prefix-map
         :prefix "M-g"
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
  :custom ((consult-ripgrep-args (string-join '("rg"
                                                "--null"
                                                "--line-buffered"
                                                "--color=never"
                                                "--max-columns" "1000"
                                                "--path-separator" "/"
                                                "--smart-case"
                                                "--no-heading"
                                                "--with-filename"
                                                "--line-number"
                                                "--search-zip"
                                                "--hidden"
                                                "--glob" "!.git"
                                                "--follow")
                                              " ")))
  :config
  (consult-customize consult-theme
                     :preview-key '(:debounce 0.2 any)))

(use-package consult
  :if (eq system-type 'gnu/linux)
  :straight nil
  :ensure-system-package
  (locate . "sudo apt install -y locate")
  (rg . "sudo apt install -y ripgrep"))

(use-package consult-company
  :disabled
  :after (consult company)
  :init (define-key company-mode-map
                    [remap completion-at-point]
                    #'consult-company))

(use-package consult-flycheck
  :after (consult flycheck)
  :bind ("M-g c" . consult-flycheck))

(use-package consult-flyspell
  :after (consult flyspell)
  :bind ("M-g s" . consult-flyspell))

(use-package consult-projectile
  :after (projectile)
  :bind ( ([remap projectile-find-dir] . consult-projectile-find-dir)
          ([remap projectile-find-file] . consult-projectile-find-file)
          ([remap projectile-switch-project] . consult-projectile-switch-project)

          :map projectile-command-map
          ("A" . consult-projectile) )
  :config
  ;; Advise `consult-projectile-switch-project' to trigger project
  ;; switch hooks. These projectile hooks won't trigger unless
  ;; `projectile-switch-project' is used.
  (defun consult-projectile-switch-project-ad (orig-func)
    (run-hooks 'projectile-before-switch-project-hook)
    (funcall orig-func)
    (run-hooks 'projectile-after-switch-project-hook))

  (advice-add #'consult-projectile-switch-project :around
              #'consult-projectile-switch-project-ad))

(provide 'subsys-consult)
;;; subsys-consult.el ends here
