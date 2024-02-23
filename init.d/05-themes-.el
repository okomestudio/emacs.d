;;; 05-themes.el --- Themes  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure Emacs themes. Visuals, colors, etc.
;;
;;; Code:

(defvar ok-themes-default-theme 'flexoki-themes-light
  "Default theme at startup.")


(use-package flexoki-themes
  :hook
  (after-load-theme
   . (lambda ()
       ;; Recover mode line box border.
       (let ((box-color (face-attribute 'mode-line :background)))
         (set-face-attribute 'mode-line nil :box box-color)
         (set-face-attribute 'mode-line-active nil :box box-color)
         (set-face-attribute 'mode-line-inactive nil :box box-color))))

  :config
  (with-eval-after-load 'corfu
    (let ((mode (frame-parameter nil 'background-mode)))
      (when (string= mode "light")
        (let ((bg "#f0f0f0"))
          (set-face-attribute 'corfu-current nil :background bg)
          (set-face-attribute 'corfu-default nil :background bg)
          (with-eval-after-load 'corfu-popupinfo
            (set-face-attribute 'corfu-popupinfo nil :background bg))))))

  (with-eval-after-load 'company
    (let ((mode (frame-parameter nil 'background-mode)))
      (when (string= mode "light")
        (set-face-attribute 'company-tooltip nil :background "#eeeeee")))))


(use-package nano-theme
  :disabled
  :straight (:host github :repo "rougier/nano-theme")
  :custom (nano-window-divider-show t))


(use-package spacemacs-theme
  :disabled
  :config
  (require 'okutil)
  (let ((mode (frame-parameter nil 'background-mode)))
    (if (string= mode "light")
        (setq spacemacs-theme-custom-colors
              `((base . "#322938") ;; #655370 for light, true-color

                ;; Make some colors slightly darker
                (head3 . ,(okutil-color-scale "#67b11d" 0.80))
                (head4 . ,(okutil-color-scale "#b1951d" 0.80))
                (cyan . ,(okutil-color-scale "#21b8c7" 0.95)))))))


;; The following utility functions assume that one and only one theme gets
;; loaded at any time.

(defvar after-load-theme-hook nil
  "Hooks to run after `load-theme'. ")

(advice-add #'load-theme :around
            (lambda (orig-fun theme &optional no-confirm no-enable)
              (mapc #'disable-theme custom-enabled-themes)
              (funcall orig-fun theme no-confirm no-enable)
              (run-hooks 'after-load-theme-hook)))


;; MODE LINE

(use-package doom-modeline
  ;; A fancy and fast mode-line inspired by minimalism design.
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-height 1)
  (doom-modeline-minor-modes t)
  (doom-modeline-vcs-max-length 12)
  (mode-line-percent-position nil)
  (doom-modeline-checker-simple-format t)

  :hook
  (after-load-theme . (lambda ()
                        (if (member (car custom-enabled-themes)
                                    '(flexoki-themes-dark
                                      flexoki-themes-light))
                            (doom-modeline-mode +1)
                          (if (default-value 'doom-modeline-mode)
                              (doom-modeline-mode -1)))))

  :config
  (doom-modeline-def-modeline 'lsp-full
    '(bar workspace-name window-number modals matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state persp-name battery grip irc mu4e gnus github debug repl minor-modes input-method indent-info buffer-encoding major-mode process vcs lsp misc-info time))

  (dolist (mode '(python-ts-mode python-sql-ts-mode))
    (add-to-list 'doom-modeline-mode-alist `(,mode . lsp-full))))


(use-package doom-nano-modeline
  :disabled
  :requires (nano-theme)
  :straight (:host github :repo "ronisbr/doom-nano-modeline")
  :config (doom-nano-modeline-mode 1))


(use-package minions
  ;; A minor-mode menu for the mode line.
  :custom (minions-direct '(projectile-mode))
  :hook (after-load-theme . (lambda () (minions-mode 1))))


;; MINOR THEME ADJUSTMENTS

(use-package solaire-mode
  ;; Distinguish "real" buffers from "unreal" buffers.
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
  :hook (prog-mode . (lambda () (rainbow-delimiters-mode 1))))


;; MISC.

(use-package ansi-color
  :straight nil
  :hook
  (compilation-filter . (lambda ()
                          (when (eq major-mode 'compilation-mode)
                            (ansi-color-apply-on-region compilation-filter-start (point-max))))))


(use-package hl-line
  ;; Highlight the current line.
  :straight nil
  :init (global-hl-line-mode 1)
  :hook
  (after-load-theme . (lambda ()
                        (let* ((mode (frame-parameter nil 'background-mode))
                               (scale (if (string= mode "dark") 1.03 0.97))
                               (bg (face-attribute 'default :background))
                               (bg-hl (okutil-color-scale bg scale)))
                          (set-face-attribute 'hl-line nil :background bg-hl)))))


(use-package highlight-indent-guides
  :disabled
  :custom
  (highlight-indent-guides-auto-enabled nil)
  (highlight-indent-guides-character ?\┆)
  (highlight-indent-guides-delay 0)
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)

  :hook
  (prog-mode . highlight-indent-guides-mode)

  :config
  (set-face-background 'highlight-indent-guides-character-face "light yellow")
  (set-face-foreground 'highlight-indent-guides-character-face "light yellow")
  (set-face-background 'highlight-indent-guides-top-character-face "light yellow")
  (set-face-foreground 'highlight-indent-guides-top-character-face "gray"))


(use-package pos-tip
  ;; Show tooltip at point.
  :custom
  (pos-tip-background-color "#dddddd")
  (pos-tip-border-width 5)
  (pos-tip-internal-border-width 5))


;; LOADER

(defun ok-themes-initial-theme (theme)
  "Set the THEME to load at Emacs startup."
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (load-theme theme t))))
    ;; (add-hook 'after-init-hook (lambda () (load-theme theme t)))
    (load-theme theme t)))


(ok-themes-initial-theme ok-themes-default-theme)

;; Local Variables:
;; nameless-aliases: (("" . "ok-themes"))
;; End:
;;; 05-themes.el ends here
