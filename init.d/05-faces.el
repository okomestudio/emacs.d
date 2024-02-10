;;; 05-faces.el --- Faces  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Font face configuration.
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
  (let* ((font-specs (--map (font-spec :family it) ok-face-cjk-font-families))
         (matched-font-spec (seq-find (lambda (font-spec)
                                        (find-font font-spec))
                                      font-specs)))
    (set-fontset-font nil 'unicode matched-font-spec frame 'append)))

(use-package 05-faces
  ;; :disabled
  :defer t
  :straight nil

  :ensure-system-package
  ("/usr/share/fonts/opentype/ebgaramond/EBGaramond08-Regular.otf" . fonts-ebgaramond)
  ("/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.ttc" . fonts-noto-cjk)
  ("/usr/share/fonts/truetype/bizud-gothic/BIZUDGothic-Regular.ttf" . fonts-morisawa-bizud-gothic)
  ("/usr/share/fonts/truetype/hack/Hack-Regular.ttf" . fonts-hack)
  ("/usr/share/fonts/truetype/vlgothic/VL-Gothic-Regular.ttf" . fonts-vlgothic)

  :hook
  (after-init . (lambda ()
                  (dolist (element ok-face-face-font-rescale-alist)
                    (add-to-list 'face-font-rescale-alist element))
                  (ok-face--set-up-action 'ok-face--setup-faces-for-frame))))


(use-package mixed-pitch
  ;; Enable mixing fixed-pitch and variable-pitch.
  :defer t

  :custom
  (mixed-pitch-variable-pitch-cursor nil)

  :hook
  (org-mode . mixed-pitch-mode)

  :config
  (delete 'org-table mixed-pitch-fixed-pitch-faces)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'font-lock-comment-face)
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-modern-bracket-line))


(use-package eaw
  ;; East Asian Ambiguous Width問題と絵文字の横幅問題の修正ロケール.
  :defer t

  :straight
  (:host github :repo "hamano/locale-eaw")

  :config
  (eaw-fullwidth))

;; Local Variables:
;; nameless-aliases: (("" . "ok-face"))
;; End:
;;; 05-faces.el ends here
