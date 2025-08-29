;;; subsys-faces.el --- Font & Faces  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure font & face subsystem.
;;
;;; Code:

(require 'dash)
(require 'ok)

(defvar ok-faces-font-family-fixed-pitch "HackGen35 Console NF"
  "Font for the fixed pitch.
This is also used as the default.")

(defvar ok-faces-font-family-fixed-pitch-ja nil ; HackGen includes Japanese
  "Font for the fixed pitch in Japanese.")

(defvar ok-faces-font-family-variable-pitch "EB Garamond"  ; "EB Garamond 08"
  "Font for the variable pitch.")

(defvar ok-faces-font-family-variable-pitch-ja "Noto Serif CJK JP Medium"
  "Font for the variable pitch in Japanese.")

;;; Utility Functions

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
                    'iso-8859-1
                    (font-spec :family ok-faces-font-family-fixed-pitch)
                    frame)
  (when ok-faces-font-family-fixed-pitch-ja
    (ok-fontset-set-font "fontset-default"
                         'ja ok-faces-font-family-fixed-pitch-ja
                         frame))
  (ok-fontset-create "fontset-fixed pitch"
                     ok-faces-font-family-fixed-pitch
                     :subsets (when ok-faces-font-family-fixed-pitch-ja
                                `((ja . ,(font-spec :family ok-faces-font-family-fixed-pitch-ja))))
                     :frame frame)
  (ok-fontset-create "fontset-variable pitch"
                     ok-faces-font-family-variable-pitch
                     :subsets (when ok-faces-font-family-variable-pitch-ja
                                `((ja . ,(font-spec :family ok-faces-font-family-variable-pitch-ja))))
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
  ;; Install font files.
  :if (eq system-type 'gnu/linux)
  :ensure-system-package
  ("/usr/share/fonts/opentype/ebgaramond/EBGaramond08-Regular.otf"
   . "sudo apt install -y fonts-ebgaramond")
  ("/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.ttc"
   . "sudo apt install -y fonts-noto-cjk")
  ;; ("/usr/share/fonts/truetype/aoyagi-kouzan-t/AoyagiKouzanT.ttf"
  ;;  . "sudo apt install -y fonts-aoyagi-kouzan-t")
  ;; ("/usr/share/fonts/truetype/bizud-gothic/BIZUDGothic-Regular.ttf"
  ;;  . "sudo apt install -y fonts-morisawa-bizud-gothic")
  ("/usr/share/fonts/truetype/umeplus/umeplus-p-gothic.ttf"
   . "sudo apt install -y fonts-umeplus")

  :init
  ;; Install fonts from GitHub:
  (let ((fonts
         `(( :family "HackGen35 Console NF"
             :url ,(file-name-concat
                    "https://github.com/okomestudio/fonts-hackgen"
                    "raw/master/hackgen-nf/HackGen35ConsoleNF-%s.ttf")
             :variants ("Bold" "Regular") )
           ( :family "URW Classico"
             :url ,(file-name-concat
                    "https://github.com/okomestudio/fonts-urw-classico"
                    "raw/master/opentype/URWClassico-%s.otf")
             :variants ("Bold" "BoldItalic" "Italic" "Regular") )))
        urls res)
    (cl-loop for font in fonts
             do
             (setq urls (flatten-list
                         (mapcar (lambda (font)
                                   (--map (format (plist-get font :url) it)
                                          (plist-get font :variants)))
                                 fonts)))
             (setq res (append res (--map (ok-font-install-from-url it) urls))))
    (when (--any? it res)
      (ok-font-cache-update)))

  ;; Set font rescaling factors:
  (dolist (element '(("HackGen35 Console NF" . 1.0) ; reference
                     ;; ("AoyagiKouzanFontT" . 1.0)
                     ;; ("BIZ UDGothic" . 1.0)
                     ("EB Garamond". 1.45)
                     ("Noto Sans CJK JP" . 1.2)
                     ("Noto Sans Mono CJK JP" . 1.2)
                     ("Noto Serif CJK JP" . 1.2)
                     ("URW Classico". 1.3)
                     ("UmePlus P Gothic" . 1.18)))
    (add-to-list 'face-font-rescale-alist element)))

(setopt ok-faces-text-scale-per-mode
        '((elfeed-search-mode . 1.0)
          (elfeed-show-mode . 0.0)
          (eww-mode . 1.0)
          (org-mode . 1.0)
          (prog-mode . 1.0)
          (text-mode . 0.0)
          (treemacs-mode . -0.4)))

(ok-faces--set-up-action (lambda () (ok-faces--setup-faces-for-frame)))

;;; Icons

(use-package nerd-icons
  ;; For a list of available icons, see, e.g., nerdfonts.ytyng.com.
  :if (member system-type '(gnu gnu/linux gnu/kfreebsd))
  :config
  (unless (file-exists-p "~/.local/share/fonts/NFM.ttf")
    (nerd-icons-install-fonts +1)))

;;; Misc.

(use-package eaw
  ;; East Asian Ambiguous Width問題と絵文字の横幅問題の修正ロケール.
  :config (eaw-fullwidth))

(use-package show-font
  :config
  (setopt show-font-character-sample
          (concat show-font-character-sample
                  "ひらがな カタカナ 漢字 ―/ダッシュ ー/長音")))

(provide 'subsys-faces)
;;; subsys-faces.el ends here
