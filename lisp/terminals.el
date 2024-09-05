;;; terminals.el --- Terminals  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Terminals configuration.
;;
;;; Code:

(use-package vterm
  ;; Emacs libvterm integration.
  :if (eq system-type 'gnu/linux)
  :bind (nil
         :map vterm-mode-map
         ("C-q" . vterm-send-next-key))
  :hook (vterm-mode . ok-vterm-configure-buffer)
  :custom
  (vterm-always-compile-module t)
  (vterm-buffer-name-string "vterm %s")
  (vterm-max-scrollback 5000)
  (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
  (vterm-timer-delay 0.02)

  ;; Need to unset this to let the shell's pyenv manage venvs:
  (vterm-environment '("PYENV_VERSION="))

  :ensure-system-package
  ("/usr/include/vterm.h" . "sudo apt install -y libvterm-dev")
  ("/usr/bin/cmake" . "sudo apt install -y cmake")
  ("/usr/bin/libtool" . "sudo apt install -y libtool-bin")

  :config
  (defun ok-vterm-configure-buffer ()
    (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
    (setq-local global-hl-line-mode nil
                solaire-mode nil)
    (buffer-face-mode t)))


(use-package multi-vterm
  :after vterm)


(use-package eat
  ;; Emulate A Terminal, in a region, in a buffer and in Eshell.
  :disabled
  :straight
  '(eat :type git
        :host codeberg
        :repo "akib/emacs-eat"
        :files ("*.el" ("term" "term/*.el") "*.texi"
                "*.ti" ("terminfo/e" "terminfo/e/*")
                ("terminfo/65" "terminfo/65/*")
                ("integration" "integration/*")
                (:exclude ".dir-locals.el" "*-tests.el")))

  :hook
  (eshell-post-command . (lambda ()
                           (sleep-for 0.2)
                           (end-of-buffer))))

;;; terminals.el ends here
