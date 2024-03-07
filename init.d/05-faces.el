;;; 05-faces.el --- faces  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure font faces and related utilities.
;;
;;; Code:

(defvar ok-faces-font-family-fixed-pitch "Hack"
  "Font for the fixed pitch.
This is also used as the default.")

(defvar ok-faces-font-family-fixed-pitch-ja "BIZ UDGothic" ;; "Noto Sans Mono CJK JP"
  "Font for the fixed pitch in Japanese.")

(defvar ok-faces-font-family-variable-pitch "EB Garamond"
  "Font for the variable pitch.")

(defvar ok-faces-font-family-variable-pitch-ja "Noto Serif CJK JP"
  "Font for the variable pitch in Japanese.")

;; UTILITY FUNCTIONS

(defun ok-faces--set-up-action (&rest action)
  "Set up ACTION to run on frame creation."
  (defun ok-faces--apply-if-gui ()
    (when (display-graphic-p)
      (select-frame (selected-frame))
      (apply action)))

  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'ok-faces--apply-if-gui)
    (ok-faces--apply-if-gui)))

(defun ok-faces-text-scale-mode-height ()
  "Get the default face height if `text-scale-mode' is active."
  (when (bound-and-true-p text-scale-mode)
    (car (cdr (assoc :height (cdr (assoc 'default face-remapping-alist)))))))

(defun ok-faces--set-fontset-font-japanese (fontset font-family &optional frame)
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

(defun ok-faces-font-exists-p (font)
  "Return t if FONT exists, nil if not."
  (if (find-font (font-spec :family font))
      t
    (message "WARNING: Font `%s' not found" font)))

(defun ok-faces-create-fontset (fontset font-family font-family-ja &optional frame)
  "Create FONTSET using FONT-FAMILY.
If FONT-FAMILY-JA is non-nil, use it for Japanese characters."
  (ok-faces-font-exists-p font-family)
  (create-fontset-from-fontset-spec
   (font-xlfd-name (font-spec :family font-family :registry fontset)))
  (when (and font-family-ja
             (ok-faces-font-exists-p font-family-ja))
    (ok-faces--set-fontset-font-japanese fontset font-family-ja frame)))

(defun ok-faces--setup-faces-for-frame (&optional frame)
  "Set up the faces for FRAME."
  ;; FONTSETS
  ;; Emacs comes with three fontsets: `fontset-startup',
  ;; `fontset-standard', and `fontset-default', the last of which is
  ;; the ultimate fallback.
  (set-fontset-font "fontset-default" 'iso-8859-3
                    (font-spec :family ok-faces-font-family-fixed-pitch) frame)
  (ok-faces--set-fontset-font-japanese "fontset-default"
                               ok-faces-font-family-fixed-pitch-ja frame)
  (ok-faces-create-fontset "fontset-fixed pitch"
                   ok-faces-font-family-fixed-pitch ok-faces-font-family-fixed-pitch-ja frame)
  (ok-faces-create-fontset "fontset-variable pitch"
                   ok-faces-font-family-variable-pitch ok-faces-font-family-variable-pitch-ja frame)

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
  )

(use-package faces
  :if (eq system-type 'gnu/linux)
  :straight nil
  :autoload (ok-faces--apply-font-rescale)
  :ensure-system-package
  ("/usr/share/fonts/opentype/ebgaramond/EBGaramond08-Regular.otf"
   . "sudo apt install -y fonts-ebgaramond")
  ("/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.ttc"
   . "sudo apt install -y fonts-noto-cjk")
  ("/usr/share/fonts/truetype/aoyagi-kouzan-t/AoyagiKouzanT.ttf"
   . "sudo apt install -y fonts-aoyagi-kouzan-t")
  ("/usr/share/fonts/truetype/bizud-gothic/BIZUDGothic-Regular.ttf"
   . "sudo apt install -y fonts-morisawa-bizud-gothic")
  ("/usr/share/fonts/truetype/hack/Hack-Regular.ttf"
   . "sudo apt install -y fonts-hack")
  ("/usr/share/fonts/truetype/vlgothic/VL-Gothic-Regular.ttf"
   . "sudo apt install -y fonts-vlgothic")

  :init
  (dolist (element '(("Hack" . 1.00) ;; reference
                     ("EB Garamond". 1.28)
                     ("BIZ UDGothic" . 1.00)
                     ("Noto Sans Mono CJK JP" . 1.18)
                     ("Noto Sans CJK JP" . 1.00)
                     ("Noto Serif CJK JP" . 1.00)
                     ;; ("VL Gothic" . 1.225)
                     ("AoyagiKouzanFontT". 1.00)))
    (push element face-font-rescale-alist)))

(use-package faces
  :straight nil
  :init
  (ok-faces--set-up-action (lambda ()
                     ;; (ok-faces--apply-font-rescale)
                     (ok-faces--setup-faces-for-frame)))
  :hook
  ;; Scale texts by mode; `text-scale-mode' affect the `default face.
  (elfeed-search-mode . (lambda () (text-scale-set 1.0)))
  (elfeed-show-mode . (lambda () (text-scale-set 0.0)))
  (eww-mode . (lambda () (text-scale-set 0.0)))
  (org-mode . (lambda ()
                (text-scale-set 0.2)
                (let ((factor (expt text-scale-mode-step
                                    text-scale-mode-amount)))
                  (plist-put org-format-latex-options
                             :scale (* 4.0 factor)))))
  (prog-mode . (lambda () (text-scale-set 0.0)))
  (text-mode . (lambda () (text-scale-set 0.0)))
  (treemacs-mode . (lambda () (text-scale-set -0.4))))

;; ICONS

(use-package nerd-icons
  :if (member system-type '(gnu gnu/linux gnu/kfreebsd))
  :config
  (unless (file-exists-p "~/.local/share/fonts/NFM.ttf")
    (nerd-icons-install-fonts +1)))


(use-package all-the-icons
  :disabled
  :if (and (display-graphic-p)
           (member system-type '(gnu gnu/linux gnu/kfreebsd)))
  :config
  (unless (file-exists-p "~/.local/share/fonts/all-the-icons.ttf")
    (all-the-icons-install-fonts +1)))

;; MISC.

(use-package mixed-pitch
  :disabled ;; ... until the package gets patched.
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
;; nameless-aliases: (("" . "ok-faces"))
;; End:
;;; 05-faces.el ends here
