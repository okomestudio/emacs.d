;;; themes.el --- Themes  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Emacs themes, visuals, colors, etc.
;;
;; See:
;;
;;   - https://emacsthemes.com/
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
  (let ((mode (frame-parameter nil 'background-mode)))
    (if (string= mode "light")
        (setq spacemacs-theme-custom-colors
              `((base . "#322938") ;; #655370 for light, true-color

                ;; Make some colors slightly darker
                (head3 . ,(ok-face-color-scale "#67b11d" 0.80))
                (head4 . ,(ok-face-color-scale "#b1951d" 0.80))
                (cyan . ,(ok-face-color-scale "#21b8c7" 0.95)))))))

;; The following utility functions assume that one and only one theme
;; gets loaded at any time.

(defvar after-load-theme-hook nil
  "Hooks to run after `load-theme'.")

(advice-add #'load-theme :around
            (lambda (orig-fun theme &optional no-confirm no-enable)
              (mapc #'disable-theme custom-enabled-themes)
              (funcall orig-fun theme no-confirm no-enable)
              (run-hooks 'after-load-theme-hook)))

;; MINOR THEME ADJUSTMENTS

(use-package solaire-mode
  ;; Distinguish "real" buffers from "unreal" buffers.
  :custom (solaire-mode-real-buffer-fn 'ok-themes--solaire-mode-real-buffer-p)
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

;; INDENTATION

(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :custom ((indent-bars-color '(highlight :face-bg t :blend 0.2))
           (indent-bars-treesit-support t)
           (indent-bars-prefer-character "|") ; see github.com/jdtsmith/indent-bars/issues/3
           (indent-bars-width-frac 0.1)
           (indent-bars-pad-frac 0.1))
  :hook ((python-ts-mode
          yaml-ts-mode) . indent-bars-mode))

(use-package highlight-indent-guides
  :disabled
  :custom ((highlight-indent-guides-auto-enabled nil)
           (highlight-indent-guides-character ?\┆)
           (highlight-indent-guides-delay 0)
           (highlight-indent-guides-method 'character)
           (highlight-indent-guides-responsive 'top))
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (set-face-background 'highlight-indent-guides-character-face "light yellow")
  (set-face-foreground 'highlight-indent-guides-character-face "light yellow")
  (set-face-background 'highlight-indent-guides-top-character-face "light yellow")
  (set-face-foreground 'highlight-indent-guides-top-character-face "gray"))

;; MISC.

(use-package olivetti
  ;; Emacs minor mode to automatically balance window margins
  )

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
                               (bg-hl (ok-face-color-scale bg scale)))
                          (set-face-attribute 'hl-line nil :background bg-hl)))))

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

(load (locate-user-emacs-file "lisp/themes-modeline.el"))

(ok-themes-initial-theme ok-themes-default-theme)

;;; themes.el ends here
