;;; themes-inactive.el --- Themes  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package highlight-indent-guides
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

;;; themes-inactive.el ends here
