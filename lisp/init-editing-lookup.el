;;; init-editing-lookup.el --- Editing-Lookup  -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;;   The collection of lookup utility exposed via a common key prefix.
;;;
;;; Code:


(require 'okutil)


(use-package init-editing-lookup
  :after (define-word powerthesaurus eww chatgpt-shell)
  :straight nil

  :bind
  (:prefix-map
   lookup-map
   :prefix-docstring "Keymap for text lookup"
   :prefix "M-L"
   ("t" . gts-do-translate)
   ("g" . ask-chatgpt)

   :prefix-map lookup-dict-map
   :prefix-docstring "Keymap for dictionary lookup"
   :prefix "M-L d"
   ("w" . define-word-at-point)
   ("p" . powerthesaurus-lookup-dwim)

   :prefix-map lookup-web-map
   :prefix-docstring "Keymap for web lookup"
   :prefix "M-L s"
   ("d" . search-weblio)
   ("g" . search-goodreads)

   :prefix-map lookup-amazon-map
   :prefix-docstring "Keymap for Amazon lookup"
   :prefix "M-L s a"
   ("e" . search-amazon)
   ("j" . search-amazon-ja)

   :prefix-map lookup-wikipedia-map
   :prefix-docstring "Keymap for Wikipedia lookup"
   :prefix "M-L s w"
   ("e" . search-wikipedia)
   ("j" . search-wikipedia-ja))

  :init
  (defun ask-chatgpt (str)
    (interactive (list (okutil-string-from-region-or-prompt "Ask ChatGPT: ")))
    (chatgpt-shell-send-to-buffer str)))


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


(use-package eww
  ;; Wrap eww to enable quicker look up in some sites

  :config
  ;; See http://emacs.rubikitch.com/eww-weblio/ for reference.
  (defun ts/eww-set-start-at (url-regexp search-regexp)
    "When site matches URL-REGEXP, start displaying from line matching SEARCH-REGEXP."
    (when (string-match url-regexp (plist-get eww-data :url))
      (goto-char (point-min))
      (when (re-search-forward search-regexp nil t)
        (recenter 0))))

  (defun ts/eww-render--after (&rest _)
    (ts/eww-set-start-at "amazon.co.jp" "^結果")
    (ts/eww-set-start-at "amazon.com" "^RESULTS")
    (ts/eww-set-start-at "goodreads.com" "^Page ")
    (ts/eww-set-start-at "en.m.wikipedia.org" "^ *Search")
    (ts/eww-set-start-at "ja.m.wikipedia.org" "^ *検索")
    (ts/eww-set-start-at "www.weblio.jp" "^ *Weblio 辞書"))

  (add-hook 'eww-after-render-hook 'ts/eww-render--after)

  (defun ts/make-query (site-url str)
    "Look up term STR at SITE-URL in eww."
    (eww-browse-url (format site-url (url-hexify-string str))))

  (defun search-amazon (str)
    (interactive (list (okutil-string-from-region-or-prompt "Amazon (US): ")))
    (ts/make-query "https://amazon.com/s?k=%s" str))

  (defun search-amazon-ja (str)
    (interactive (list (okutil-string-from-region-or-prompt "Amazon (JP): ")))
    (ts/make-query "https://amazon.co.jp/s?k=%s" str))

  (defun search-goodreads (str)
    (interactive (list (okutil-string-from-region-or-prompt "Goodreads: ")))
    (ts/make-query "https://goodreads.com/search?q=%s" str))

  (defun search-weblio (str)
    (interactive (list (okutil-string-from-region-or-prompt "Weblio: ")))
    (ts/make-query "https://www.weblio.jp/content/%s" (upcase str)))

  (defun search-wikipedia (str)
    (interactive (list (okutil-string-from-region-or-prompt "Wikipedia (en): ")))
    (ts/make-query "https://en.m.wikipedia.org/wiki/%s" str))

  (defun search-wikipedia-ja (str)
    (interactive (list (okutil-string-from-region-or-prompt "Wikipedia (ja): ")))
    (ts/make-query "https://ja.m.wikipedia.org/wiki/%s" str)))


(use-package go-translate
  ;; A translation framework.
  :defer t

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


(provide 'init-editing-lookup)
;;; init-editing-lookup.el ends here
