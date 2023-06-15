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

  :ensure-system-package
  ("/usr/share/fonts/opentype/ebgaramond/EBGaramond08-Regular.otf" . fonts-ebgaramond)
  ("/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.ttc" . fonts-noto-cjk)
  ("/usr/share/fonts/truetype/hack/Hack-Regular.ttf" . fonts-hack)
  ("/usr/share/fonts/truetype/vlgothic/VL-Gothic-Regular.ttf" . fonts-vlgothic)

  ;; Change text scale within some modes
  :hook
  (elfeed-search-mode . (lambda () (text-scale-set 1.5)))
  (elfeed-show-mode . (lambda () (text-scale-set 1.5)))
  (org-mode . (lambda () (text-scale-set 1.5)))
  (prog-mode . (lambda () (text-scale-set 0.5)))
  (text-mode . (lambda () (text-scale-set 0.5)))
  (treemacs-mode . (lambda () (text-scale-decrease 0.4)))

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


(use-package mixed-pitch
  ;; Enable mixing fixed-pitch and variable-pitch
  :hook (org-mode . mixed-pitch-mode)
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
                                   org-checkbox
                                   org-formula
                                   org-meta-line
                                   ;; org-table
                                   org-verbatim)
                                 )
  )


(use-package eaw
  ;; East Asian Ambiguous Width問題と絵文字の横幅問題の修正ロケール.
  ;;
  ;; See https://github.com/hamano/locale-eaw.
  ;;
  :straight nil
  :config (eaw-fullwidth)
  :init (ensure-file-from-github "hamano/locale-eaw/master/eaw.el"))

(use-package nerd-icons
  :config
  (if (not (file-exists-p "~/.local/share/fonts/NFM.ttf"))
      (nerd-icons-install-fonts +1)))

(use-package all-the-icons
  :disabled
  :if (display-graphic-p)
  :init
  (if (not (file-exists-p "~/.local/share/fonts/all-the-icons.ttf"))
      (all-the-icons-install-fonts +1)))

(provide 'init-faces)
;;; init-faces.el ends here
