;;; init-consult.el --- Consult  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package consult
  ;; Consulting completing-read.
  :straight
  (:pre-build
   ;; build info manual, which appears missing by default:
   ("emacs" "-Q" "-batch" "-L" "./" "--visit" "README.org" "--funcall" "org-texinfo-export-to-texinfo")
   ("makeinfo" "consult.texi" "-o" "consult.info")
   ("install-info" "consult.info" "dir"))

  :bind
  (:prefix "M-g"
   :prefix-map consult-prefix-map
   ("b" . consult-bookmark)
   ("f" . consult-find)
   ("F" . consult-locate)
   ("g" . consult-git-grep)
   ("G" . consult-ripgrep)
   ("i" . consult-imenu)
   ("I" . consult-imenu-multi)
   ("l" . consult-line)
   ("L" . consult-line-multi)
   ("m" . consult-mark)
   ("M" . consult-global-mark)
   ("M-g" . consult-goto-line)
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

  :init
  ;; (global-set-key [remap apropos] #'consult-apropos)
  (global-set-key [remap electric-apropos] #'consult-apropos)
  (global-set-key [remap switch-to-buffer-other-window] #'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer] #'consult-buffer)

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
  (("C-c p A" . consult-projectile)
   ([remap projectile-find-dir] . consult-projectile-find-dir)
   ([remap projectile-find-file] . consult-projectile-find-file)
   ([remap projectile-switch-project] . consult-projectile-switch-project)))

(provide 'init-consult)
;;; init-consult.el ends here
