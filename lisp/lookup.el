;;; lookup.el --- Lookup  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; The collection of lookup utility exposed via a common key prefix.
;;
;;; Code:

(use-package emacs
  :straight nil
  :bind (;
         :prefix-map lookup-map
         :prefix-docstring "Keymap for lookup"
         :prefix "C-h C-l"
         ("c" . ask-chatgpt)
         ("g" . eww-search-goodreads)
         ("t" . gt-do-translate)

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

;; DICTIONARIES

(use-package define-word
  ;; Display the definition of word at point.
  :custom (define-word-default-service 'wordnik))

(use-package powerthesaurus
  ;; Pull from doomelpa to avoid aggressive `transient' load issue (#41)
  :straight (:host github :repo "doomelpa/powerthesaurus"))

(use-package synosaurus
  ;; An extensible thesaurus mode.
  :disabled
  :custom (synosaurus-choose-method 'default)
  :ensure-system-package (wn . "sudo apt install -y wordnet"))

;; PRONUNCIATION

(use-package hatsuon
  :straight (:host github :repo "okomestudio/hatsuon.el"
                   :files (:defaults "extensions/*"))
  :custom ((hatsuon-audio-cache-dir (no-littering-expand-var-file-name "hatsuon/cache/"))
           (hatsuon-audio-url-getters '(hatsuon-mw-audio-url-getter
                                        ;; hatsuon-wiktionary-audio-url-getter
                                        ;; hatsuon-weblio-audio-url-getter
                                        )))
  :config
  (require 'hatsuon-mw)
  (require 'hatsuon-weblio))

;; TRANSLATION

(use-package go-translate
  ;; Translator framework.
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
             :render (gt-buffer-render)))))

;; MISC.

(use-package list-unicode-display
  ;; Search for and list unicode characters.
  ;;
  ;; Type `list-unicode-display' to search for unicode chars.
  )

(use-package greppu
  :straight (:host github :repo "okomestudio/greppu.el")
  :commands (greppu-scan)
  :preface
  (put 'greppu-search-regexp-pcre 'safe-local-variable #'functionp))

;;; lookup.el ends here
