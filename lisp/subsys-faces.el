;;; subsys-faces.el --- Faces Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the font subsystem.
;;
;;; Code:

(require 'ok)

(defvar ok-faces-font-family-fixed-pitch "Hack"
  "Font for the fixed pitch.
This is also used as the default.")

(defvar ok-faces-font-family-fixed-pitch-ja "BIZ UDGothic"  ; "Noto Sans Mono CJK JP"
  "Font for the fixed pitch in Japanese.")

(defvar ok-faces-font-family-variable-pitch "EB Garamond"  ; "EB Garamond 08"
  "Font for the variable pitch.")

(defvar ok-faces-font-family-variable-pitch-ja "Noto Serif CJK JP Medium"
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

(defun ok-faces--setup-faces-for-frame (&optional frame)
  "Set up the faces for FRAME."
  ;; FONTSETS
  ;;
  ;; Emacs comes with three fontsets: `fontset-startup',
  ;; `fontset-standard', and `fontset-default', the last of which is
  ;; the ultimate fallback.
  (set-fontset-font "fontset-default"
                    'iso-8859-3
                    (font-spec :family ok-faces-font-family-fixed-pitch)
                    frame)
  (ok-fontset-set-font "fontset-default"
                       'ja
                       ok-faces-font-family-fixed-pitch-ja
                       frame)
  (ok-fontset-create "fontset-fixed pitch"
                     ok-faces-font-family-fixed-pitch
                     :subsets `((ja ,(font-spec :family ok-faces-font-family-fixed-pitch-ja)))
                     :frame frame)
  (ok-fontset-create "fontset-variable pitch"
                     ok-faces-font-family-variable-pitch
                     :subsets `((ja ,(font-spec :family ok-faces-font-family-variable-pitch-ja)))
                     :frame frame)

  ;; STANDARD FACES
  (set-face-attribute 'default frame
                      :family ok-faces-font-family-fixed-pitch
                      ;; :font "fontset-default"
                      :fontset "fontset-default")
  (set-face-attribute 'bold frame :weight 'bold)
  (set-face-attribute 'italic frame :slant 'italic :underline nil)
  (set-face-attribute 'bold-italic frame :weight 'bold :slant 'italic)
  (set-face-attribute 'underline frame :underline t)
  (set-face-attribute 'fixed-pitch frame
                      :family ok-faces-font-family-fixed-pitch
                      ;; :font "fontset-fixed pitch"
                      :fontset "fontset-fixed pitch")
  (set-face-attribute 'variable-pitch frame
                      :family ok-faces-font-family-variable-pitch
                      ;; :font "fontset-variable pitch"
                      :fontset "fontset-variable pitch")
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
  (dolist (element '(("Hack" . 1.00)      ; reference
                     ("EB Garamond". 1.6) ; 1.28
                     ("BIZ UDGothic" . 1.00)
                     ("Noto Sans Mono CJK JP" . 1.18) ; 1.18
                     ("Noto Sans CJK JP" . 1.10)
                     ("Noto Serif CJK JP" . 1.26)
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
  (org-mode . (lambda () (text-scale-set 0.8)))
  (prog-mode . (lambda () (text-scale-set 0.0)))
  (text-mode . (lambda () (text-scale-set 0.0)))
  (treemacs-mode . (lambda () (text-scale-set -0.4))))

;; ICONS

(use-package nerd-icons
  :if (member system-type '(gnu gnu/linux gnu/kfreebsd))
  :config
  (unless (file-exists-p "~/.local/share/fonts/NFM.ttf")
    (nerd-icons-install-fonts +1)))

;; MISC.

(use-package eaw
  ;; East Asian Ambiguous Width問題と絵文字の横幅問題の修正ロケール.
  :straight (:host github :repo "hamano/locale-eaw")
  :config (eaw-fullwidth))

(use-package show-font
  :config
  (setopt show-font-character-sample
          (concat show-font-character-sample
                  "ひらがな カタカナ 漢字 ―/ダッシュ ー/長音")))

(provide 'subsys-faces)
;;; subsys-faces.el ends here
