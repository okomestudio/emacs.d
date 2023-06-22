;;; init-consult.el --- Consult  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package consult
  ;; Consulting completing-read.
  :ensure-system-package
  ((locate . "sudo apt install -y locate"))

  :init
  (global-set-key (kbd "M-g b") 'consult-bookmark)
  (global-set-key (kbd "M-g f") 'consult-find)
  (global-set-key (kbd "M-g F") 'consult-locate)
  (global-set-key (kbd "M-g g") 'consult-git-grep)
  (global-set-key (kbd "M-g G") 'consult-grep)
  (global-set-key (kbd "M-g i") 'consult-imenu)
  (global-set-key (kbd "M-g I") 'consult-imenu-multi)
  (global-set-key (kbd "M-g l") 'consult-line)
  (global-set-key (kbd "M-g L") 'consult-line-multi)
  (global-set-key (kbd "M-g m") 'consult-mark)
  (global-set-key (kbd "M-g M") 'consult-gloabl-mark)
  (global-set-key (kbd "M-g M-g") 'consult-goto-line)
  (global-set-key [remap apropos] #'consult-apropos)
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
