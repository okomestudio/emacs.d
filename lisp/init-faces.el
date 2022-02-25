;;; init-faces.el --- Faces  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package faces
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

  (defun ts/font-exists-p (font)
    "Check if font exists."
    (if (null (x-list-fonts font)) nil t))

  (defun ts/create-cjk-hybrid-fontset (size name)
    "Create a CJK hybrid fontset of SIZE named fontset-NAME

See https://knowledge.sakura.ad.jp/8494/"
    (let ((font-spec (format "Hack:weight=normal:slant=normal:size=%d" size))
          (fontset-name (format "fontset-%s" name)))
      (create-fontset-from-ascii-font font-spec nil name)
      (ts/set-fallback-cfk-font fontset-name)
      fontset-name))

  (defun ts/set-fallback-cjk-font (fontset-name)
    (let ((font-family (seq-find #'ts/font-exists-p '("HackGen"
                                                      "VL Gothic"
                                                      "Noto Sans Mono CJK JP"))))
      (set-fontset-font fontset-name
                        'unicode
                        (font-spec :family font-family)
                        nil
                        'append)))

  (defun ts/get-display-width ()
    "Get the pixel with per display."
    (let ((monn (length (display-monitor-attributes-list))))
      (/ (display-pixel-width) monn)))

  (defun ts/setup-frame ()
    (defvar ts/display-width (ts/get-display-width))
    (defvar ts/font-size (if (and ts/display-width (> ts/display-width 2550)) 18 10.8))
    (defvar ts/default-font (font-spec :family "Hack" :size ts/font-size))
    (set-frame-font ts/default-font)
    (ts/set-fallback-cjk-font nil))

  (ts/apply-if-gui 'ts/setup-frame))

(provide 'init-faces)
;;; init-faces.el ends here
