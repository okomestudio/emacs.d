;;; init-editing-lookup.el --- Editing-Lookup  -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;;   The collection of lookup utility exposed via a common key prefix.
;;;
;;; Code:


(use-package init-editing-lookup
  :straight nil
  :after (define-word powerthesaurus eww google-translate)
  :bind
  (
   :prefix "M-L"
   :prefix-map text-map
   :prefix-docstring "Keymap for editing lookup"
   ("d w" . define-word-at-point)
   ("d p" . powerthesaurus-lookup-dwim)
   ("t g" . google-translate-smooth-translate)
   ("w a e" . search-amazon)
   ("w a j" . search-amazon-jp)
   ("w w e" . search-wikipedia)
   ("w w j" . search-wikipedia-jp)
   ("w d" . search-weblio)
   )
  )


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
    (ts/eww-set-start-at "en.m.wikipedia.org" "^ *Search")
    (ts/eww-set-start-at "ja.m.wikipedia.org" "^ *検索")
    (ts/eww-set-start-at "www.weblio.jp" "^ *Weblio 辞書"))
  (add-hook 'eww-after-render-hook 'ts/eww-render--after)

  (defun ts/region-or-read-string (prompt &optional initial history default inherit)
    "When region is specified, use it as string; otherwise, get it interactively."
    (if (not (region-active-p))
        (read-string prompt initial history default inherit)
      (prog1
          (buffer-substring-no-properties (region-beginning) (region-end))
        (deactivate-mark)
        (message ""))))

  (defun ts/make-query (site-url str)
    "Look up term STR at SITE-URL in eww."
    (eww-browse-url (format site-url (url-hexify-string str))))

  (defun search-amazon (str)
    (interactive (list (ts/region-or-read-string "Amazon (US): ")))
    (ts/make-query "https://amazon.com/s?k=%s" str))

  (defun search-amazon-ja (str)
    (interactive (list (ts/region-or-read-string "Amazon (JP): ")))
    (ts/make-query "https://amazon.co.jp/s?k=%s" str))

  (defun search-weblio (str)
    (interactive (list (ts/region-or-read-string "Weblio: ")))
    (ts/make-query "https://www.weblio.jp/content/%s" (upcase str)))

  (defun search-wikipedia (str)
    (interactive (list (ts/region-or-read-string "Wikipedia (en): ")))
    (ts/make-query "https://en.m.wikipedia.org/wiki/%s" str))

  (defun search-wikipedia-ja (str)
    (interactive (list (ts/region-or-read-string "Wikipedia (ja): ")))
    (ts/make-query "https://ja.m.wikipedia.org/wiki/%s" str)))


(use-package google-translate
  ;; Emacs interface to Google Translate.

  :config
  (require 'google-translate)
  (require 'google-translate-smooth-ui)

  ;; (defun google-translate--search-tkk ()
  ;;   "Search TKK."
  ;;   (list 430675 2721866130))

  (setq google-translate-backend-method 'curl
        google-translate-output-destination nil
        google-translate-translation-directions-alist '(("en" . "ja") ("ja" . "en")) )
  )

(provide 'init-editing-lookup)
;;; init-editing-lookup.el ends here
