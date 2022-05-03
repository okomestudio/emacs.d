;;; init-faces.el --- Faces  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun ts/display-width ()
    "Get the pixel with per display."
    (let ((monn (length (display-monitor-attributes-list))))
      (/ (display-pixel-width) monn)))

(defcustom ts/default-font-size
  (let ((display-width (ts/display-width)))
      (if (and display-width (> display-width 2550))
          18.0 10.8))
  "Default font size."
  :type '(float)
  :group 'ts)

(defcustom ts/fixed-pitch-height
  (let ((display-width (ts/display-width)))
    (if (and display-width (> display-width 2550))
        180 150))
  "Default fixed pitch font height."
  :type '(integer)
  :group 'ts)

(defcustom ts/variable-pitch-height
  (let ((display-width (ts/display-width)))
    (if (and display-width (> display-width 2550))
        180 180))
  "Default variable pitch font height."
  :type '(integer)
  :group 'ts)

(defun ts/font-exists-p (font)
  "Check if FONT exists."
  (if (null (x-list-fonts font)) nil t))

(defun ts/set-fallback-cjk-font (fontset-name)
  (let ((font-family (seq-find #'ts/font-exists-p '(;; "HackGen"
                                                    "VL Gothic"
                                                    "Noto Sans Mono CJK JP"))))
    (set-fontset-font fontset-name
                      'unicode
                      (font-spec :family font-family)
                      nil
                      'append)))


(use-package faces
  :disabled
  :ensure nil

  :init
  (defun ts/apply-if-gui (&rest action)
    "Apply ACTION if we are in a GUI."
    (if (daemonp)
        (add-hook 'server-after-make-frame-hook
                  (lambda ()
                    (let ((frame (selected-frame)))
                      (select-frame frame)
                      (if (display-graphic-p frame)
                          (apply action)))))
      (if (display-graphic-p)
          (progn
            (select-frame (selected-frame))
            (apply action)))))

  (defun ts/create-cjk-hybrid-fontset (size name)
    "Create a CJK hybrid fontset of SIZE named fontset-NAME

See https://knowledge.sakura.ad.jp/8494/"
    (let ((font-spec (format "Hack:weight=normal:slant=normal:size=%d" size))
          (fontset-name (format "fontset-%s" name)))
      (create-fontset-from-ascii-font font-spec nil name)
      (ts/set-fallback-cfk-font fontset-name)
      fontset-name))

  (defun ts/setup-frame ()
    (defvar ts/default-font (font-spec :family "Hack" :size ts/default-font-size))
    (set-frame-font ts/default-font)
    (ts/set-fallback-cjk-font nil))

  (ts/apply-if-gui 'ts/setup-frame)

  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (select-frame frame)
              (ts/apply-if-gui 'ts/setup-frame))))

(use-package mixed-pitch
  :hook (text-mode . mixed-pitch-mode)

  :config
  (set-face-attribute 'default nil :font "Hack" :height ts/fixed-pitch-height)
  (set-face-attribute 'fixed-pitch nil :font "Hack" :height ts/fixed-pitch-height)
  (set-face-attribute 'variable-pitch nil :font "EB Garamond" :height ts/variable-pitch-height)
  (ts/set-fallback-cjk-font nil)

  (dolist (element '(("VL Gothic" . 0.85)))
    (add-to-list 'face-font-rescale-alist element))

  :custom
  (mixed-pitch-set-height t)
  (mixed-pitch-variable-pitch-cursor nil))

;; eaw.el - East Asian Ambiguous Width問題と絵文字の横幅問題の修正ロケール
;; https://github.com/hamano/locale-eaw
(use-package eaw
  :ensure nil
  :config (eaw-fullwidth)
  :init (ensure-file-from-github "hamano/locale-eaw/master/eaw.el"))

(provide 'init-faces)
;;; init-faces.el ends here
