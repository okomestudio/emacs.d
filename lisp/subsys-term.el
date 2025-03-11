;;; subsys-term.el --- Terminal Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up terminal subsystem.
;;
;;; Code:

;;; vterm - Emacs libvterm integration

(use-package vterm
  ;; Emacs libvterm integration.
  :if (eq system-type 'gnu/linux)
  :bind (:map
         vterm-mode-map
         ("C-q" . vterm-send-next-key))
  :custom ((vterm-always-compile-module t)
           (vterm-buffer-name-string "vterm %s")
           (vterm-max-scrollback 5000)
           (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
           (vterm-timer-delay 0.02)

           ;; Need to unset this to let the shell's pyenv manage venvs:
           (vterm-environment '("PYENV_VERSION=")))
  :ensure-system-package
  ("/usr/include/vterm.h" . "sudo apt install -y libvterm-dev")
  ("/usr/bin/cmake" . "sudo apt install -y cmake")
  ("/usr/bin/libtool" . "sudo apt install -y libtool-bin")
  :hook (vterm-mode . vterm-ok-buffer-configure)
  :config
  (defun vterm-ok-buffer-configure ()
    (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
    (setq-local global-hl-line-mode nil
                solaire-mode nil)
    (buffer-face-mode t)))

(use-package multi-vterm
  :after vterm)

;;; EAT - Emulate A Terminal

(use-package eat
  ;; Emulate A Terminal, in a region, in a buffer and in Eshell.
  ;;
  ;; Eat input modes:
  ;;
  ;; Semi-char mode is the default. To input exception keys, prefix
  ;; with `C-q'. Also, `C-c C-c' sends `C-c', and `C-c C-k' kills the
  ;; terminal program.
  ;;
  ;; In char mode, Eat forwards all supported keys to terminals. Type
  ;; `C-c M-d' to switch to this mode, and `C-M-m' or `M-<RET>' to
  ;; switch back to semi-char mode.
  ;;
  ;; In Emacs mode, the buffer acts like a regular Emacs buffer. Type
  ;; `C-c C-e' to switch to this mode, and `C-c C-j' to switch back to
  ;; semi-char mode or `C-c M-d' to char mode.
  ;;
  :straight (eat :type git
                 :host codeberg
                 :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el")))
  :bind (("C-S-t" . eat-ok-interactively)
         :map eat-semi-char-mode-map
         ("M-o" . other-window-or-frame)) ; without explicit
                                          ; definition, it binds to
                                          ; another function
  :hook ((eat-exec . eat-ok--rename-buffer)
         (eshell-post-command . (lambda ()
                                  (sleep-for 0.2)
                                  (end-of-buffer))))
  :config
  (defun eat-ok--rename-buffer (&rest _)
    "Rename EAT buffer based on the parent directory name."
    (rename-buffer
     (format "*eat-%s*" (car (last (file-name-split default-directory) 2)))
     t))

  (defun eat-ok-interactively (arg)
    "When prefixed, call `eat-project'; without prefix, call `eat'."
    (interactive "P")
    (pcase arg
      ('(4) (eat-project))
      (_ (eat)))))

(provide 'subsys-term)
;;; subsys-term.el ends here
