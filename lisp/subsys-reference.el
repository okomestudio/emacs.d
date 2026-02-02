;;; subsys-reference.el --- Reference  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the reference subsystem.
;;
;; The collection of reference utility exposed via a common key prefix.
;;
;;; Code:

(require 'ok)

(with-eval-after-load 'help
  (bind-keys :prefix-map reference-map
             :prefix-docstring "Keymap for reference"
             :prefix "C-h C-l"
             ("a" . gptel-send)
             ("p" . lookup-pattern)
             ("r" . lookup-region)
             ("t" . gt-translate)
             ("w" . ok-eww-search-web)

             :prefix-map reference-en-map
             :prefix-docstring "Keymap for English reference"
             :prefix "C-h C-l e"
             ("d" . define-word-at-point)
             ("h" . hatsuon-play-audio)
             ("p" . powerthesaurus-lookup-dwim)

             :prefix-map reference-ja-map
             :prefix-docstring "Keymap for Japanese reference"
             :prefix "C-h C-l j"
             ("k" . kakijun)))

;;; Dictionaries

(use-package define-word
  ;; Display the definition of word at point.
  :custom (define-word-default-service 'wordnik))

(use-package powerthesaurus
  ;; Pull from doomelpa to avoid aggressive `transient' load issue (#41)
  )

(use-package synosaurus
  ;; An extensible thesaurus mode.
  :disabled
  :custom (synosaurus-choose-method 'default)
  :ensure-system-package (wn . "sudo apt install -y wordnet"))

(use-package urbandict.el)

;;; Kakijun

(defun kakijun (s)
  "Visit the kakijun.com page for each kanji character in string S."
  (interactive "sInput kanji characters: ")
  (letrec ((url nil) (urls nil)
           (eww-make-readable
            (lambda ()
              (eww-readable)
              (setq urls (delete (eww-current-url) urls))
              (unless urls
                (remove-hook 'eww-after-render-hook eww-make-readable)))))
    (add-hook 'eww-after-render-hook eww-make-readable)
    (dolist (c (string-to-list s))
      (setq url (format "https://kakijun.com/c/%x.html" c))
      (push url urls)
      (eww url t))))

;;; EPWING

(use-package eblook)

(use-package lookup
  :custom ((lookup-max-hits 1000)
           (lookup-window-height 16)
           (lookup-use-kakasi t)

           ;; For ndeb, the path should point to a directory containing
           ;; CATALOGS.
           (lookup-search-agents nil))
  :hook ((lookup-content-mode . lookup-ok--set-window-size)

         ;; `lookup' skips `after-change-major-mode-hook', so ensure the text
         ;; scaling runs:
         (lookup-content-mode . ok-face-text-scale-per-mode))
  :config
  (load (ok-file-expand-etc "lookup/init"))

  (defun lookup-ok--set-window-size ()
    (toggle-truncate-lines 1)
    (let ((target-width (round (* 0.6 (frame-width)))))
      (window-resize (selected-window)
                     (- target-width (window-width))
                     t)))

  (with-eval-after-load 'ok-face
    (add-to-list 'ok-face-text-scale-per-mode '(lookup-content-mode . 1.5))))

;;; Pronunciation

(use-package hatsuon
  :disabled                   ; use `gt.el'
  :custom ((hatsuon-audio-cache-dir (ok-file-expand-var "hatsuon/cache/"))
           (hatsuon-audio-url-getters
            '(hatsuon-wordnik-audio-url-getter)))
  :config (require 'hatsuon-wordnik))

;;; Translation

(use-package gt
  ;; Translator framework.
  ;;
  ;; For audio pronunciation, use "y" shortcut (`gt-do-speak') within
  ;; the `gt-buffer'.
  ;;
  :custom ((gt-buffer-render-split-width-threshold 120)
           (gt-debug-p nil)
           (gt-langs '(en ja))
           (gt-default-translator
            (gt-translator
             :taker (gt-taker :prompt t)
             :engines (list
                       (gt-deepl-engine)
                       (gt-google-engine :parse (gt-google-summary-parser))
                       (gt-google-rpc-engine :parse (gt-google-rpc-parser))
                       (gt-bing-engine))
             :render (gt-buffer-render))))
  :config
  ;; Implement the at-point hendler for Org links.
  (cl-defmethod gt-thing-at-point ((_ (eql 'org-link)) (_ (eql 'org-mode)))
    (let* ((link (org-element-context))
           (beg (org-element-property :contents-begin link))
           (end (org-element-property :contents-end link))
           (desc (buffer-substring beg end))
           bds)
      (push (cons beg end) bds)))

  (setq gt-preset-translators
        `((org-google . ,(gt-translator
                          :taker (gt-taker :text 'org-link)
                          :engines (gt-google-engine)
                          :render (gt-insert-render
                                   :rfmt (lambda (res)
                                           (format " (%s)" res))
                                   :sface nil)))
          (org-deepl . ,(gt-translator
                         :taker (gt-taker :text 'org-link)
                         :engines (gt-deepl-engine)
                         :render (gt-insert-render
                                  :rfmt (lambda (res)
                                          (format " (%s)" res))
                                  :sface nil))))))

;;; Misc.

(use-package list-unicode-display
  ;; Search for and list unicode characters.
  ;;
  ;; Type `list-unicode-display' to search for unicode chars.
  )

(use-package greppu
  :commands (greppu-scan))

(use-package wiktionary-bro
  ;; Etymology lookup/Wiktionary browser.
  :commands (wiktionary-bro))

(provide 'subsys-reference)
;;; subsys-reference.el ends here
