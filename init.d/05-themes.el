;;; 05-themes.el --- Themes  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure Emacs themes. Visuals, colors, etc.
;;
;;; Code:

(defun ok-themes--configure-theme (theme)
  "Configure and load THEME."
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (load-theme theme t))))
    (load-theme theme t)))


;; THEME

(use-package flexoki-themes
  :config (ok-themes--configure-theme 'flexoki-themes-light))


(use-package nano-theme
  :disabled
  :straight (:host github :repo "rougier/nano-theme")
  :config (ok-themes--configure-theme 'nano))


(use-package spacemacs-theme
  :disabled
  :defer t
  :init
  (require 'okutil)
  (setq spacemacs-theme-custom-colors
        `((base . "#322938") ;; #655370 for light, true-color

          ;; Make some colors slightly darker
          (head3 . ,(okutil-color-scale '(#x67 #xb1 #x1d) 0.80))
          (head4 . ,(okutil-color-scale '(#xb1 #x95 #x1d) 0.80))
          (cyan . ,(okutil-color-scale '(#x21 #xb8 #xc7) 0.95))))

  (ok-themes--configure-theme 'spacemacs-light))


;; MODE LINE

(use-package doom-modeline
  ;; A fancy and fast mode-line inspired by minimalism design.
  :defer t
  :after nerd-icons
  :requires (flexoki-themes spacemacs-theme)
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


(use-package doom-nano-modeline
  :disabled
  :requires (nano-theme)
  :straight (:host github :repo "ronisbr/doom-nano-modeline")
  :config (doom-nano-modeline-mode 1))


(use-package minions
  ;; A minor-mode menu for the mode line.
  :custom (minions-direct '(projectile-mode))
  :hook (after-init . (lambda () (minions-mode 1))))


;; MINOR THEME ADJUSTMENTS

(use-package solaire-mode
  ;; Distinguish "real" buffers from "unreal" buffers.
  :defer t

  :custom
  (solaire-mode-real-buffer-fn 'ok-themes--solaire-mode-real-buffer-p)

  :preface
  (defun ok-themes--solaire-mode-real-buffer-p ()
    (cond ((string-prefix-p "*elfeed" (buffer-name)) t)
          ((string-prefix-p "*Colors*" (buffer-name)) nil)
          (t (solaire-mode-real-buffer-p))))

  :config
  (solaire-global-mode +1))


(use-package rainbow-delimiters
  ;; Highlights delimiters such as parentheses, brackets or braces according to
  ;; their depth.
  :defer t

  :hook
  (prog-mode . rainbow-delimiters-mode))


;; MISC.

(use-package hl-line
  ;; Highlight the current line.
  :straight nil

  :hook
  (after-init . (lambda () (global-hl-line-mode +1)))

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

;; Local Variables:
;; nameless-aliases: (("" . "ok-themes"))
;; End:
;;; 05-themes.el ends here
