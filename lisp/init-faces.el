;;; init-faces.el --- Faces  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require 'okutil)


(defcustom ts/default-font-size nil
  "Default font size."
  :type '(float)
  :group 'ts)


(defcustom ts/default-font-height nil
  "Default font height."
  :type '(integer)
  :group 'ts)


(defun ts/display-width ()
  "Get the pixel with per display monitor."
  (/ (display-pixel-width) (okutil-monitor-count)))


(defun ts/default-font-size ()
  "Get default font size."
  (if ts/default-font-size
      ts/default-font-size
    (if (> (ts/display-width) 2550)
        18.0
      10.8)))


(defun ts/default-font-height ()
  "Get default font height."
  (if ts/default-font-height
      ts/default-font-height
    (if (> (ts/display-width) 2550)
        130
      110)))


(defun ts/font-exists-p (font)
  "Check if FONT exists."
  (if (null (x-list-fonts font))
      nil
    t))


(use-package faces
  :straight nil

  :ensure-system-package
  ("/usr/share/fonts/opentype/ebgaramond/EBGaramond08-Regular.otf" . fonts-ebgaramond)
  ("/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.ttc" . fonts-noto-cjk)
  ("/usr/share/fonts/truetype/bizud-gothic/BIZUDGothic-Regular.ttf" . fonts-morisawa-bizud-gothic)
  ("/usr/share/fonts/truetype/hack/Hack-Regular.ttf" . fonts-hack)
  ("/usr/share/fonts/truetype/vlgothic/VL-Gothic-Regular.ttf" . fonts-vlgothic)

  :preface
  (setq ts/font-family-default "Hack"
        ts/font-family-fixed-ptch "Hack"
        ts/font-family-variable-pitch "EB Garamond"
        ts/font-family-cjk-fallbacks '("BIZ UDGothic"
                                       "Noto Sans Mono CJK JP"
                                       "VL Gothic"))

  ;; Set relative scales for font faces here. For best alignment, try with fixed
  ;; pitch font so that two ASCII characters have the same width with a CJK
  ;; character.
  (setq ts/font-relative-scales '(("Hack" . 1.0) ; use as reference size
                                  ("EB Garamond". 1.4)
                                  ("BIZ UDGothic" . 1.225)
                                  ("Noto Sans Mono CJK JP" . 1.225)
                                  ("VL Gothic" . 1.225)))

  (defun init-faces--apply-if-gui (&rest action)
    "Apply ACTION if we are in a GUI."
    (if (daemonp)
        (add-hook 'server-after-make-frame-hook
                  (lambda ()
                    (let ((frame (selected-frame)))
                      (select-frame frame)
                      (if (display-graphic-p frame)
                          (apply action)))))
      (when (display-graphic-p)
        (select-frame (selected-frame))
        (apply action))))

  (defun ts/set-fallback-cjk-font (fontset-name)
    (let ((font-family (seq-find #'ts/font-exists-p
                                 ts/font-family-cjk-fallbacks)))
      (set-fontset-font fontset-name
                        'unicode
                        (font-spec :family font-family)
                        nil
                        'append)))

  (defun ts/setup-font-for-frame ()
    (set-face-attribute 'default nil :family ts/font-family-default)
    (set-face-attribute 'fixed-pitch nil :family ts/font-family-fixed-ptch)
    (set-face-attribute 'variable-pitch nil :family ts/font-family-variable-pitch)
    (ts/set-fallback-cjk-font nil)

    (set-face-attribute 'italic nil :slant 'italic :underline nil)
    (set-face-attribute 'underline nil :slant 'normal :underline t))

  (defun ts/create-cjk-hybrid-fontset (size name)
    "Create a CJK hybrid fontset of SIZE named fontset-NAME

See https://knowledge.sakura.ad.jp/8494/"
    (let ((font-spec (format "Hack:weight=normal:slant=normal:size=%d" size))
          (fontset-name (format "fontset-%s" name)))
      (create-fontset-from-ascii-font font-spec nil name)
      (ts/set-fallback-cfk-font fontset-name)
      fontset-name))

  :init
  (dolist (element ts/font-relative-scales)
    (add-to-list 'face-font-rescale-alist element))

  (init-faces--apply-if-gui 'ts/setup-font-for-frame))


(use-package mixed-pitch
  ;; Enable mixing fixed-pitch and variable-pitch.
  :custom
  (mixed-pitch-variable-pitch-cursor nil)
  (mixed-pitch-fixed-pitch-faces '(diff-added
                                   diff-context
                                   diff-file-header
                                   diff-function
                                   diff-header
                                   diff-hunk-header
                                   diff-removed
                                   font-latex-math-face
                                   font-latex-sedate-face
                                   font-latex-warning-face
                                   font-latex-sectioning-5-face
                                   font-lock-builtin-face
                                   font-lock-comment-delimiter-face
                                   font-lock-constant-face
                                   font-lock-doc-face
                                   font-lock-function-name-face
                                   font-lock-keyword-face
                                   font-lock-negation-char-face
                                   font-lock-preprocessor-face
                                   font-lock-regexp-grouping-backslash
                                   font-lock-regexp-grouping-construct
                                   font-lock-string-face
                                   font-lock-type-face
                                   font-lock-variable-name-face
                                   line-number
                                   line-number-current-line
                                   line-number-major-tick
                                   line-number-minor-tick
                                   markdown-code-face
                                   markdown-gfm-checkbox-face
                                   markdown-inline-code-face
                                   markdown-language-info-face
                                   markdown-language-keyword-face
                                   markdown-math-face
                                   message-header-name
                                   message-header-to
                                   message-header-cc
                                   message-header-newsgroups
                                   message-header-xheader
                                   message-header-subject
                                   message-header-other
                                   mu4e-header-key-face
                                   mu4e-header-value-face
                                   mu4e-link-face
                                   mu4e-contact-face
                                   mu4e-compose-separator-face
                                   mu4e-compose-header-face
                                   org-block
                                   org-block-begin-line
                                   org-block-end-line
                                   org-document-info-keyword
                                   org-code
                                   org-indent
                                   org-latex-and-related
                                   org-modern-bracket-line
                                   org-checkbox
                                   org-formula
                                   org-meta-line
                                   ;; org-table
                                   org-verbatim))

  :hook
  (org-mode . mixed-pitch-mode))


(use-package eaw
  ;; East Asian Ambiguous Width問題と絵文字の横幅問題の修正ロケール.
  ;;
  ;; See https://github.com/hamano/locale-eaw.
  ;;
  :straight
  (:host github :repo "hamano/locale-eaw")

  :config
  (eaw-fullwidth))


(use-package font-core
  :straight nil

  :config
  (add-hook 'switch-buffer-functions
            (lambda (_pref current)
              ;; NOTE: This hook is for turning off font-lock-mode in
              ;; list-colors-display only.
              (when (string-equal (buffer-name current)
                                  "*Colors*")
                (font-lock-mode -1)
                (list-colors-display)))))


(provide 'init-faces)
;;; init-faces.el ends here
