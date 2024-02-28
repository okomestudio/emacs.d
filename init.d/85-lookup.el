;;; 85-lookup.el --- Lookup  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; The collection of lookup utility exposed via a common key prefix.
;;
;;; Code:

(use-package emacs
  :straight nil
  :bind
  (nil
   :prefix-map lookup-map
   :prefix-docstring "Keymap for lookup"
   :prefix "C-h C-l"
   ("c" . ask-chatgpt)
   ("g" . eww-search-goodreads)
   ("t" . gts-do-translate)

   :prefix-map lookup-english-map
   :prefix-docstring "Keymap for English lookup"
   :prefix "C-h C-l e"
   ("a" . eww-search-amazon-en)
   ("d" . define-word-at-point)
   ("h" . hatsuon-play-audio)
   ("j" . eww-search-justapedia-en)
   ("p" . powerthesaurus-lookup-dwim)
   ("s" . eww-search-duckduckgo-en)
   ("w" . eww-search-wikipedia-en)

   :prefix-map lookup-japanese-map
   :prefix-docstring "Keymap for Japanese lookup"
   :prefix "C-h C-l j"
   ("a" . eww-search-amazon-ja)
   ("d" . eww-search-weblio)
   ("s" . eww-search-duckduckgo-ja)
   ("w" . eww-search-wikipedia-ja)))

;; Dictionaries

(use-package define-word
  ;; Display the definition of word at point.
  :custom (define-word-default-service 'wordnik))


(use-package powerthesaurus
  ;; Powerthesaurus integration.
  )


(use-package synosaurus
  ;; An extensible thesaurus mode.
  :disabled
  :custom (synosaurus-choose-method 'default)
  :ensure-system-package (wn . "sudo apt install -y wordnet"))


;; Pronunciation

(use-package hatsuon
  :straight
  (:host github :repo "okomestudio/hatsuon.el" :branch "main"))


;; Translation

(use-package go-translate
  ;; A translation framework.
  :custom
  (gts-split-width-threshold 120)
  (gts-translate-list '(("en" "ja") ("ja" "en")))

  :config
  (setq deepl-authkey (plist-get (car (auth-source-search
                                       :host "deepl.com"
                                       :requires '(:authkey)))
                                 :authkey))

  (setq gts-custom-engines
        (list (gts-google-engine :parser (gts-google-summary-parser))
              (gts-google-rpc-engine :parser (gts-google-rpc-parser))
              (gts-bing-engine)))

  ;; If the auth key exists, add DeepL engine:
  (unless (null deepl-authkey)
    (push (gts-deepl-engine :auth-key deepl-authkey :pro nil)
          gts-custom-engines))

  (setq gts-default-translator (gts-translator
                                :picker (gts-prompt-picker)
                                :engines gts-custom-engines
                                :render (gts-buffer-render))))


;; Misc.

(use-package list-unicode-display
  ;; Search for and list unicode characters.
  ;;
  ;; Type list-unicode-display to search for unicode chars.
  )


(use-package greppu
  :straight (:host github :repo "okomestudio/greppu.el")
  :commands (greppu-scan)
  :preface
  (put 'greppu-search-regexp-pcre 'safe-local-variable #'functionp))

;;; 85-lookup.el ends here
