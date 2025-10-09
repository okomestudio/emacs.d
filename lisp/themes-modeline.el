;;; themes-modeline.el --- Modeline Theme  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the modeline theme.
;;
;;; Code:

(require 's)

(use-package doom-modeline
  ;; A fancy and fast mode-line inspired by minimalism design.
  :custom ((doom-modeline-buffer-encoding nil)
           (doom-modeline-buffer-file-name-style 'buffer-name)
           (doom-modeline-check-simple-format t)
           (doom-modeline-height 1)
           (doom-modeline-minor-modes t)
           (doom-modeline-vcs-max-length 12)
           (mode-line-percent-position nil)
           (doom-modeline-checker-simple-format t))
  :hook (enable-theme-functions . doom-modeline-ok--enable)
  :config
  (cl-defun doom-modeline-ok--enable (&rest _)
    (which-function-mode -1)  ; enable to show function/section name in modeline
    (doom-modeline-mode 1))

  (defsubst doom-modeline--buffer-mode-icon-or-text ()
    "Show buffer-mode-icon or major-mode segment."
    (let ((help-echo (string-join `(,(format "Major mode: %s" mode-name)
                                    "mouse-1: Display major mode menu"
                                    "mouse-2: Show help for major mode"
                                    "mouse-3: Toggle minor modes")
                                  "\n")))
      (if (doom-modeline--buffer-mode-icon)
          (propertize (doom-modeline--buffer-mode-icon)
                      'help-echo help-echo
                      'local-map mode-line-major-mode-keymap)
        (propertize (format-mode-line
                     (or (and (boundp 'delighted-modes)
                              (cadr (assq major-mode delighted-modes)))
                         mode-name))
                    'help-echo help-echo
                    'face (doom-modeline-face 'doom-modeline-buffer-major-mode)
                    'mouse-face 'doom-modeline-highlight
                    'local-map mode-line-major-mode-keymap))))

  (doom-modeline-def-segment env
    "Environment."
    (when (and doom-modeline-env-version doom-modeline-env--version)
      (format "[%s]" doom-modeline-env--version)))

  (doom-modeline-def-segment text-scale
    "Text scale."
    (if-let* ((amount (and (boundp 'text-scale-mode-amount)
                           text-scale-mode-amount)))
        (concat
         " "
         (propertize
          (s-repeat (abs (pcase amount (0 1) (_ amount)))
                    (cond ((> amount 0) "+") ((< amount 0) "-") (t ".")))
          'face (doom-modeline-face 'doom-modeline-buffer-major-mode)))
      ""))

  (doom-modeline-def-segment buffer-major-mode
    "Buffer major mode."
    (let ((face (doom-modeline-face 'doom-modeline-buffer-major-mode)))
      (concat
       ;; (doom-modeline-spc)
       (doom-modeline--buffer-mode-icon-or-text))))

  (doom-modeline-def-segment buffer-state
    "Buffer state, e.g., file name and state."
    (concat
     ;; (doom-modeline-spc)
     (doom-modeline--buffer-state-icon)
     (doom-modeline--buffer-name)))

  (doom-modeline-def-segment vcs-simple
    "VCS info."
    (if-let* ((it (doom-modeline-update-vcs)))
        (list (car it) '(text . ""))))

  ;; See the `doom-modeline-def-segment' forms in `doom-modeline-segments' for
  ;; available modeline segments and how to customize them.
  (doom-modeline-def-modeline 'lsp-full
    '( bar workspace-name window-number modals matches follow
       buffer-major-mode buffer-state text-scale
       remote-host buffer-position word-count
       parrot selection-info )
    '( compilation objed-state persp-name battery grip
       irc mu4e gnus github debug repl
       input-method minor-modes
       indent-info buffer-encoding
       env vcs-simple lsp misc-info time ))

  (add-to-list 'doom-modeline-mode-alist '(prog-mode . lsp-full))
  (add-to-list 'doom-modeline-mode-alist '(text-mode . lsp-full)))

(use-package minions
  ;; Consolidate minor mode info into an icon.
  :custom (minions-direct '(projectile-mode))
  :hook (enable-theme-functions . minions-ok--enable)
  :config
  (cl-defun minions-ok--enable (&rest _)
    (minions-mode 1)))

(provide 'themes-modeline)
;;; themes-modeline.el ends here
