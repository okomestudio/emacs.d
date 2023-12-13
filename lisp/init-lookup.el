;;; init-lookup.el --- Lookup  -*- lexical-binding: t -*-
;;; Commentary:
;;
;;   The collection of lookup utility exposed via a common key prefix.
;;
;;; Code:


(require 'okutil)


(use-package init-lookup
  :after (define-word powerthesaurus eww chatgpt-shell)
  :straight nil

  :bind
  (:prefix-map lookup-map
   :prefix-docstring "Keymap for lookup"
   :prefix "M-L"
   ("c" . ask-chatgpt)
   ("g" . search-goodreads)
   ("t" . gts-do-translate)

   :prefix-map lookup-english-map
   :prefix-docstring "Keymap for English lookup"
   :prefix "M-L e"
   ("a" . search-amazon-en)
   ("d" . define-word-at-point)
   ("e" . search-duckduckgo-en)
   ("j" . search-justapedia-en)
   ("p" . powerthesaurus-lookup-dwim)
   ("w" . search-wikipedia-en)

   :prefix-map lookup-japanese-map
   :prefix-docstring "Keymap for Japanese lookup"
   :prefix "M-L j"
   ("a" . search-amazon-ja)
   ("d" . search-weblio)
   ("e" . search-duckduckgo-ja)
   ("w" . search-wikipedia-ja)))


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
    "Move to the specified line on page load."
    (ts/eww-set-start-at "amazon.co.jp" "^結果")
    (ts/eww-set-start-at "amazon.com" "^RESULTS")
    (ts/eww-set-start-at "b.hatena.ne.jp" "^記事へのコメント")
    (ts/eww-set-start-at "goodreads.com" "^Page ")
    (ts/eww-set-start-at "en.m.wikipedia.org" "^ *Search")
    (ts/eww-set-start-at "ja.m.wikipedia.org" "^ *検索")
    (ts/eww-set-start-at "www.weblio.jp" "^ *Weblio 辞書"))

  (add-hook 'eww-after-render-hook 'ts/eww-render--after)

  (defun init-lookup--make-query (site-url str)
    "Look up term STR at SITE-URL in eww."
    (eww-browse-url (format site-url (url-hexify-string str))))

  (defun search-amazon-en (str)
    (interactive (list (okutil-string-from-region-or-prompt "Amazon (US): ")))
    (init-lookup--make-query "https://amazon.com/s?k=%s" str))

  (defun search-amazon-ja (str)
    (interactive (list (okutil-string-from-region-or-prompt "Amazon (JP): ")))
    (init-lookup--make-query "https://amazon.co.jp/s?k=%s" str))

  (defun search-duckduckgo-en (str)
    (interactive (list (okutil-string-from-region-or-prompt "DuckDuckGo (en): ")))
    (init-lookup--make-query "https://html.duckduckgo.com/html/?q=%s&kp=-2&kl=wt-wt&ks=s" str))

  (defun search-duckduckgo-ja (str)
    (interactive (list (okutil-string-from-region-or-prompt "DuckDuckGo (ja): ")))
    (init-lookup--make-query "https://html.duckduckgo.com/html/?q=%s&kp=-2&kl=jp-jp&ks=s" str))

  (defun search-goodreads (str)
    (interactive (list (okutil-string-from-region-or-prompt "Goodreads: ")))
    (init-lookup--make-query "https://goodreads.com/search?q=%s" str))

  (defun search-weblio (str)
    (interactive (list (okutil-string-from-region-or-prompt "Weblio: ")))
    (init-lookup--make-query "https://www.weblio.jp/content/%s" (upcase str)))

  (defun search-wikipedia-en (str)
    (interactive (list (okutil-string-from-region-or-prompt "Wikipedia (en): ")))
    (init-lookup--make-query "https://en.m.wikipedia.org/wiki/%s" str))

  (defun search-wikipedia-ja (str)
    (interactive (list (okutil-string-from-region-or-prompt "Wikipedia (ja): ")))
    (init-lookup--make-query "https://ja.m.wikipedia.org/wiki/%s" str))

  (defun search-justapedia-en (str)
    (interactive (list (okutil-string-from-region-or-prompt "Justapedia (en): ")))
    (init-lookup--make-query "https://justapedia.org/wiki/%s" str)))


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


(provide 'init-lookup)
;;; init-lookup.el ends here
