;;; linux-05-faces.el --- faces  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure font faces and related utilities.
;;
;;; Code:

(defvar ok-face-font-family-fixed-pitch "Hack"
  "Font for the fixed pitch.
This is also used as the default.")

(defvar ok-face-font-family-fixed-pitch-ja "BIZ UDGothic" ;; "Noto Sans Mono CJK JP"
  "Font for the fixed pitch in Japanese.")

(defvar ok-face-font-family-variable-pitch "EB Garamond"
  "Font for the variable pitch.")

(defvar ok-face-font-family-variable-pitch-ja "Noto Serif CJK JP"
  "Font for the variable pitch in Japanese.")

(defvar ok-face-font-family-outline "URW Classico"
  "Font for outlines.")

(defvar ok-face-font-family-outline-ja "Noto Sans CJK JP"
  "Font for outlines in Japanese.")

(defvar ok-face-face-font-rescale-alist '(("Hack" . 1.00) ;; reference
                                   ("EB Garamond". 1.28)
                                   ("BIZ UDGothic" . 1.00)
                                   ("URW Classico" . 1.28)
                                   ("Noto Sans Mono CJK JP" . 1.18)
                                   ("Noto Sans CJK JP" . 1.00)
                                   ("Noto Serif CJK JP" . 1.00)
                                   ;; ("VL Gothic" . 1.225)
                                   ("AoyagiKouzanFontT". 1.00))
  "Set relative scales for font faces.
For best alignment, try with fixed pitch font so that two ASCII
characters have the same width with a CJK character.")

(defface ok-face-outline '((t :inherit 'default))
  "Face for outlines.
Use when contrast with non-outline contenst is desired."
  :group 'ok)


;; UTILITY FUNCTIONS

(defun ok-face--set-up-action (&rest action)
  "Set up ACTION to run on frame creation."
  (defun ok-face--apply-if-gui ()
    (when (display-graphic-p)
      (select-frame (selected-frame))
      (apply action)))

  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'ok-face--apply-if-gui)
    (ok-face--apply-if-gui)))

(defun ok-face--create-fontset (font-family fontset)
  "Create FONTSET using FONT-FAMILY."
  (create-fontset-from-fontset-spec
   (font-xlfd-name (font-spec :family font-family :registry fontset))))

(defun ok-face--set-fontset-font-japanese (fontset font-family &optional frame)
  "Modify FONTSET to use FONT-FAMILY for Japanese rendering."
  (dolist (characters '(japanese-jisx0208
                        japanese-jisx0208-1978
                        japanese-jisx0212
                        japanese-jisx0213-1
                        japanese-jisx0213-2
                        japanese-jisx0213-a
                        japanese-jisx0213.2004-1
                        jisx0201
                        latin-jisx0201
                        katakana-jisx0201
                        katakana-sjis))
    (set-fontset-font fontset characters (font-spec :family font-family) frame)))

(defun ok-face--setup-faces-for-frame (&optional frame)
  "Set up the faces for FRAME."
  (dolist (font `(,ok-face-font-family-fixed-pitch
                  ,ok-face-font-family-fixed-pitch-ja
                  ,ok-face-font-family-variable-pitch
                  ,ok-face-font-family-variable-pitch-ja
                  ,ok-face-font-family-outline
                  ,ok-face-font-family-outline-ja))
    (if (not (find-font (font-spec :family font)))
        (message "WARNING: Font `%s' not found" font)))

  (dolist (element ok-face-face-font-rescale-alist)
    (push element face-font-rescale-alist))

  ;; FONTSETS
  ;; Emacs comes with three fontsets: `fontset-startup',
  ;; `fontset-standard', and `fontset-default', the last of which is
  ;; the ultimate fallback.
  (ok-face--create-fontset ok-face-font-family-fixed-pitch "fontset-fixed pitch")
  (ok-face--create-fontset ok-face-font-family-variable-pitch "fontset-variable pitch")
  (ok-face--create-fontset ok-face-font-family-outline "fontset-urw classico")

  ;; Modify fontset for multi-lingual support.
  (set-fontset-font "fontset-default" 'iso-8859-3 (font-spec :family ok-face-font-family-fixed-pitch) frame)
  (ok-face--set-fontset-font-japanese "fontset-default" ok-face-font-family-fixed-pitch-ja frame)
  (ok-face--set-fontset-font-japanese "fontset-fixed pitch" ok-face-font-family-fixed-pitch-ja frame)
  (ok-face--set-fontset-font-japanese "fontset-variable pitch" ok-face-font-family-variable-pitch-ja frame)
  (ok-face--set-fontset-font-japanese "fontset-urw classico" ok-face-font-family-outline-ja frame)

  ;; STANDARD FACES
  (set-face-attribute 'default frame :height 120 :font "fontset-default" :fontset "fontset-default")
  (set-face-attribute 'bold frame :weight 'bold)
  (set-face-attribute 'italic frame :slant 'italic :underline nil)
  (set-face-attribute 'bold-italic frame :weight 'bold :slant 'italic)
  (set-face-attribute 'underline frame :underline t)
  (set-face-attribute 'fixed-pitch frame :font "fontset-fixed pitch" :fontset "fontset-fixed pitch")
  ;; (set-face-attribute 'fixed-pitch-serif :height 240)
  (set-face-attribute 'variable-pitch frame :font "fontset-variable pitch" :fontset "fontset-variable pitch")
  ;; (set-face-attribute 'variable-pitch-text :height 240)
  (set-face-attribute 'shadow frame :inherit 'default)
  ;; See the Standard Faces section of Emacs manual for the rest of
  ;; the commonly used faces.

  ;; CUSTOM FACES
  (set-face-attribute 'ok-face-outline frame :font "fontset-urw classico" :fontset "fontset-urw classico"))


(use-package emacs
  :straight nil
  :ensure-system-package
  ("/usr/share/fonts/opentype/ebgaramond/EBGaramond08-Regular.otf" . fonts-ebgaramond)
  ("/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.ttc" . fonts-noto-cjk)
  ("/usr/share/fonts/truetype/aoyagi-kouzan-t/AoyagiKouzanT.ttf". fonts-aoyagi-kouzan-t)
  ("/usr/share/fonts/truetype/bizud-gothic/BIZUDGothic-Regular.ttf" . fonts-morisawa-bizud-gothic)
  ("/usr/share/fonts/truetype/hack/Hack-Regular.ttf" . fonts-hack)
  ("/usr/share/fonts/truetype/vlgothic/VL-Gothic-Regular.ttf" . fonts-vlgothic)

  :init (ok-face--set-up-action 'ok-face--setup-faces-for-frame)
  :hook
  ;; Scale texts by mode; `text-scale-mode' affect the `default face.
  (elfeed-search-mode . (lambda () (text-scale-set 0.0)))
  (elfeed-show-mode . (lambda () (text-scale-set 0.0)))
  (eww-mode . (lambda () (text-scale-set 0.0)))
  (org-mode
   . (lambda ()
       (text-scale-set 0.2)
       (let ((factor (expt text-scale-mode-step text-scale-mode-amount)))
         (plist-put org-format-latex-options :scale (* 3.33 factor)))))
  (prog-mode . (lambda () (text-scale-set 0.0)))
  (text-mode . (lambda () (text-scale-set 0.0)))
  (treemacs-mode . (lambda () (text-scale-set -0.25))))


(use-package mixed-pitch
  :disabled ;; ... until the package gets patched.
  :straight (:host github :repo "okomestudio/mixed-pitch" :fork "okomestudio")
  :hook (org-mode . mixed-pitch-mode)
  :custom (mixed-pitch-variable-pitch-cursor nil)
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
