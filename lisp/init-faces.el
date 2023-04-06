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

(defcustom ts/default-font-height
  (let ((display-width (ts/display-width)))
    (if (and display-width (> display-width 2550))
        130 110))
  "Default font height."
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
  :straight nil

  ;; Change text scale within some modes
  :hook
  (elfeed-search-mode . (lambda () (text-scale-set 1.5)))
  (elfeed-show-mode . (lambda () (text-scale-set 1.5)))
  (org-mode . (lambda () (text-scale-set 1.5)))
  (prog-mode . (lambda () (text-scale-set 0.5)))
  (text-mode . (lambda () (text-scale-set 0.5)))

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

  ;;   (defun ts/create-cjk-hybrid-fontset (size name)
  ;;     "Create a CJK hybrid fontset of SIZE named fontset-NAME

  ;; See https://knowledge.sakura.ad.jp/8494/"
  ;;     (let ((font-spec (format "Hack:weight=normal:slant=normal:size=%d" size))
  ;;           (fontset-name (format "fontset-%s" name)))
  ;;       (create-fontset-from-ascii-font font-spec nil name)
  ;;       (ts/set-fallback-cfk-font fontset-name)
  ;;       fontset-name))

  ;;   (defun ts/setup-frame ()
  ;;     (defvar ts/default-font (font-spec :family "Hack" :size ts/default-font-size))
  ;;     (set-frame-font ts/default-font)
  ;;     (ts/set-fallback-cjk-font nil))

  (defun ts/setup-font-for-frame ()
    (set-face-attribute 'default nil :family "Hack")
    (set-face-attribute 'fixed-pitch nil :family "Hack")
    (set-face-attribute 'variable-pitch nil :family "EB Garamond")
    (ts/set-fallback-cjk-font nil))

  ;; Set relative scales for font faces here. For best alignment, try with fixed
  ;; pitch font so that two ASCII characters have the same width with a CJK
  ;; character.
  (dolist (element '(("Hack" . 1.0)
                     ("VL Gothic" . 1.225)
                     ("EB Garamond". 1.4)))
    (add-to-list 'face-font-rescale-alist element))

  (ts/apply-if-gui 'ts/setup-font-for-frame))

;; mixed-pitch -Enable mixing fixed-pitch and variable-pitch
;; https://gitlab.com/jabranham/mixed-pitch
(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :custom (mixed-pitch-variable-pitch-cursor nil))

;; eaw.el - East Asian Ambiguous Width問題と絵文字の横幅問題の修正ロケール
;; https://github.com/hamano/locale-eaw
(use-package eaw
  :straight nil
  :config (eaw-fullwidth)
  :init (ensure-file-from-github "hamano/locale-eaw/master/eaw.el"))

(provide 'init-faces)
;;; init-faces.el ends here
