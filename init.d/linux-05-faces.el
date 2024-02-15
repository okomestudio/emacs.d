;;; linux-05-faces.el --- Faces  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure font faces and utilities.
;;
;;; Code:

(defvar ok-face-font-family-default "Hack"
  "Font family for default face.")

(defvar ok-face-font-family-fixed-pitch "Hack"
  "Font family for fixed pitch face.")

(defvar ok-face-font-family-variable-pitch "EB Garamond"
  "Font family for variable pitch face.")

(defvar ok-face-cjk-font-families '("BIZ UDGothic"
                             "Noto Sans Mono CJK JP"
                             "VL Gothic")
  "CJK fonts to look for in order.

The first one available will be picked for CJK characters.")

(defvar ok-face-face-font-rescale-alist '(("Hack" . 1.0) ; reference
                                   ("EB Garamond". 1.4)
                                   ("BIZ UDGothic" . 1.225)
                                   ("Noto Sans Mono CJK JP" . 1.225)
                                   ("VL Gothic" . 1.225))
  "Set relative scales for font faces.

For best alignment, try with fixed pitch font so that two ASCII
characters have the same width with a CJK character.")

(defun ok-face--set-up-action (&rest action)
  "Set up ACTION to run at the correct timing."
  (defun ok-face--apply-if-gui ()
    (when (display-graphic-p)
      (select-frame (selected-frame))
      (apply action)))

  (if (daemonp)
      (add-hook 'server-after-make-frame-hook ok-face--apply-if-gui)
    (ok-face--apply-if-gui)))

(defun ok-face--create-cjk-hybrid-fontset (size name)
  "Create a CJK hybrid fontset named fontset-NAME from an ASCII font.

SIZE is the font size of the ASCII font. This could be used
instead of `set-fontset-font' for CJK chars. TODO: Make this
fully functiona.

See: https://knowledge.sakura.ad.jp/8494/"
  (let ((ascii-font (format "Hack:weight=normal:slant=normal:size=%d" size))
        (fontset-name (format "fontset-%s" name)))
    (create-fontset-from-ascii-font ascii-font nil fontset-name)))

(defun ok-face--setup-faces-for-frame (&optional frame)
  "Set up default faces for the frame."
  (set-face-attribute 'default frame :family ok-face-font-family-default)
  (set-face-attribute 'fixed-pitch frame :family ok-face-font-family-fixed-pitch)
  (set-face-attribute 'variable-pitch frame :family ok-face-font-family-variable-pitch)

  ;; Look for a CJK font and use it to render UNICODE chars in this frame:
  (require 'dash)
  (let* ((font-specs (--map (font-spec :family it) ok-face-cjk-font-families))
         (matched-font-spec (seq-find (lambda (font-spec)
                                        (find-font font-spec))
                                      font-specs)))
    (set-fontset-font nil 'unicode matched-font-spec frame 'append))

  ;; NOTE: By default, italic is rendered as underline, for which this is a fix.
  (set-face-attribute 'italic nil :slant 'italic :underline nil)
  (set-face-attribute 'underline nil :slant 'normal :underline t))

(use-package emacs
  :straight nil
  :ensure-system-package
  ("/usr/share/fonts/opentype/ebgaramond/EBGaramond08-Regular.otf" . fonts-ebgaramond)
  ("/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.ttc" . fonts-noto-cjk)
  ("/usr/share/fonts/truetype/bizud-gothic/BIZUDGothic-Regular.ttf" . fonts-morisawa-bizud-gothic)
  ("/usr/share/fonts/truetype/hack/Hack-Regular.ttf" . fonts-hack)
  ("/usr/share/fonts/truetype/vlgothic/VL-Gothic-Regular.ttf" . fonts-vlgothic)

  :init
  (dolist (element ok-face-face-font-rescale-alist)
    (push element face-font-rescale-alist))
  (ok-face--set-up-action 'ok-face--setup-faces-for-frame)
  
  :hook
  ;; Scale texts by mode
  (elfeed-search-mode . (lambda () (text-scale-set 1.0)))
  (elfeed-show-mode . (lambda () (text-scale-set 1.5)))
  (eww-mode . (lambda () (text-scale-set 1.5)))
  (org-mode
   . (lambda ()
       (text-scale-set 1.5)
       (let ((factor (expt text-scale-mode-step text-scale-mode-amount)))
         (plist-put org-format-latex-options :scale (* 3.33 factor)))))
  (prog-mode . (lambda () (text-scale-set 0.5)))
  (text-mode . (lambda () (text-scale-set 0.5)))
  (treemacs-mode . (lambda () (text-scale-decrease 0.4))))


(use-package mixed-pitch
  ;; Enable mixing fixed-pitch and variable-pitch.
  :custom
  (mixed-pitch-variable-pitch-cursor nil)

  :hook
  (org-mode . mixed-pitch-mode)

  :config
  (delete 'org-table mixed-pitch-fixed-pitch-faces)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'font-lock-comment-face)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-modern-bracket-line)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-modern-tag))


(use-package eaw
  ;; East Asian Ambiguous Width問題と絵文字の横幅問題の修正ロケール.
  :straight (:host github :repo "hamano/locale-eaw")
  :config (eaw-fullwidth))

;; Local Variables:
;; nameless-aliases: (("" . "ok-face"))
;; End:
;;; linux-05-faces.el ends here
