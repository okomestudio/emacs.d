;;; init-themes.el --- Themes  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package spacemacs-theme
  :defer t

  :preface
  (defun ts/configure-theme (theme)
    "Configure and load THEME."
    (if (daemonp)
        (add-hook 'after-make-frame-functions
                  (lambda (frame)
                    (with-selected-frame frame
                      (load-theme theme t))))
      (load-theme theme t)))

  (defun init-themes--scale-color (orig factor)
    (let* ((result 0.0))
      (cl-loop for pow downfrom 2
               for x in orig
               do
               (setq result
                     (+ result (* (expt 256 pow) (round (* x factor))))))
      (format "#%X" result)))

  :init
  (setq spacemacs-theme-custom-colors
        `((base . "#322938")  ; #655370 for light, true-color

          ;; Make some colors slightly darker
          (head3 . ,(init-themes--scale-color '(#x67 #xb1 #x1d) 0.80))
          (head4 . ,(init-themes--scale-color '(#xb1 #x95 #x1d) 0.80))
          (cyan . ,(init-themes--scale-color '(#x21 #xb8 #xc7) 0.95))))

  (ts/configure-theme 'spacemacs-light))


(use-package solaire-mode
  ;; Distinguish "real" buffers from "unreal" buffers.
  :custom
  (solaire-mode-real-buffer-fn 'ts/solaire-mode-real-buffer-p)

  :preface
  (defun ts/solaire-mode-real-buffer-p ()
    (cond ((string-prefix-p "*elfeed" (buffer-name)) t)
          ((string-prefix-p "*Colors*" (buffer-name)) nil)
          (t (solaire-mode-real-buffer-p))))

  :config
  (solaire-global-mode +1))


(use-package rainbow-delimiters
  ;; Highlights delimiters such as parentheses, brackets or braces according to
  ;; their depth.
  :hook
  (prog-mode . rainbow-delimiters-mode))


;; MODE-LINE

(use-package doom-modeline
  ;; A fancy and fast mode-line inspired by minimalism design.
  :after nerd-icons
  :demand t

  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-height 1)
  (doom-modeline-minor-modes t)
  (doom-modeline-vcs-max-length 12)
  (mode-line-percent-position nil)

  :hook
  (after-init . doom-modeline-mode))


(use-package minions
  ;; A minor-mode menu for the mode line.
  :custom
  (minions-direct '(projectile-mode))

  :config
  (minions-mode 1))


;; MISC.

(use-package hl-line
  ;; Highlight the current line.
  :straight nil

  :init
  (global-hl-line-mode +1)

  :config
  (set-face-attribute 'hl-line nil
                      :inherit nil
                      :background "lemon chiffon"))


(use-package highlight-indent-guides
  :disabled

  :custom
  (highlight-indent-guides-auto-enabled nil)
  (highlight-indent-guides-character ?\┆)
  (highlight-indent-guides-delay 0)
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)

  :hook
  ((prog-mode) . highlight-indent-guides-mode)

  :config
  (set-face-background 'highlight-indent-guides-character-face "light yellow")
  (set-face-foreground 'highlight-indent-guides-character-face "light yellow")
  (set-face-background 'highlight-indent-guides-top-character-face "light yellow")
  (set-face-foreground 'highlight-indent-guides-top-character-face "gray"))


(use-package pos-tip
  ;; Show tooltip at point.
  :disabled

  :init
  (setq pos-tip-background-color "white")
  (if (and (boundp 'ts/font-size) ts/font-size)
      (setq pos-tip-internal-border-width
            (truncate (* ts/font-size 1.5)))))


(provide 'init-themes)
;;; init-themes.el ends here
