;;; eww.el --- EWW  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Emacs's builtin web browser.
;;
;;; Code:

(use-package eww
  :config
  (defun eww-reddit-redirect(url)
    "Redirect reddit.com to old.reddit.com automatically.
See reddit.com/r/emacs/comments/1bcf8v3."
    (replace-regexp-in-string "https://www.reddit.com"
                              "https://old.reddit.com"
                              url))

  (setopt eww-url-transformers '(eww-remove-tracking eww-reddit-redirect))

  (defun ok-eww--set-start-at (url-regexp search-regexp)
    "When site matches URL-REGEXP, start displaying from line matching SEARCH-REGEXP.
See http://emacs.rubikitch.com/eww-weblio/ for reference."
    (when (string-match url-regexp (plist-get eww-data :url))
      (goto-char (point-min))
      (when (re-search-forward search-regexp nil t)
        (recenter 0))))

  (defun ok-eww--render-after (&rest _)
    "Move to the specified line on page load."
    (ok-eww--set-start-at "amazon.co.jp" "^結果")
    (ok-eww--set-start-at "amazon.com" "^RESULTS")
    (ok-eww--set-start-at "b.hatena.ne.jp" "^記事へのコメント")
    (ok-eww--set-start-at "goodreads.com" "^Page ")
    (ok-eww--set-start-at "en.m.wikipedia.org" "^ *Search")
    (ok-eww--set-start-at "ja.m.wikipedia.org" "^ *検索")
    (ok-eww--set-start-at "www.weblio.jp" "^ *Weblio 辞書"))

  (add-hook 'eww-after-render-hook 'ok-eww--render-after)

  ;; Wrap eww to enable quicker look up in some sites.
  (defun ok-eww--make-query (site-url str)
    "Look up term STR at SITE-URL in eww."
    (eww-browse-url (format site-url (url-hexify-string str))))

  (defun eww-search-amazon-en (str)
    (interactive (list (ok-prompt-or-string-from-region "Amazon (US): ")))
    (ok-eww--make-query "https://amazon.com/s?k=%s" str))

  (defun eww-search-amazon-ja (str)
    (interactive (list (ok-prompt-or-string-from-region "Amazon (JP): ")))
    (ok-eww--make-query "https://amazon.co.jp/s?k=%s" str))

  (defun eww-search-duckduckgo-en (str)
    (interactive (list (ok-prompt-or-string-from-region "DuckDuckGo (en): ")))
    (ok-eww--make-query "https://html.duckduckgo.com/html/?q=%s&kp=-2&kl=wt-wt&ks=s" str))

  (defun eww-search-duckduckgo-ja (str)
    (interactive (list (ok-prompt-or-string-from-region "DuckDuckGo (ja): ")))
    (ok-eww--make-query "https://html.duckduckgo.com/html/?q=%s&kp=-2&kl=jp-jp&ks=s" str))

  (defun eww-search-goodreads (str)
    (interactive (list (ok-prompt-or-string-from-region "Goodreads: ")))
    (ok-eww--make-query "https://goodreads.com/search?q=%s" str))

  (defun eww-search-weblio (str)
    (interactive (list (ok-prompt-or-string-from-region "Weblio: ")))
    (ok-eww--make-query "https://www.weblio.jp/content/%s" (upcase str)))

  (defun eww-search-wikipedia-en (str)
    (interactive (list (ok-prompt-or-string-from-region "Wikipedia (en): ")))
    (ok-eww--make-query "https://en.m.wikipedia.org/wiki/%s" str))

  (defun eww-search-wikipedia-ja (str)
    (interactive (list (ok-prompt-or-string-from-region "Wikipedia (ja): ")))
    (ok-eww--make-query "https://ja.m.wikipedia.org/wiki/%s" str))

  (defun eww-search-justapedia-en (str)
    (interactive (list (ok-prompt-or-string-from-region "Justapedia (en): ")))
    (ok-eww--make-query "https://justapedia.org/wiki/%s" str)))


(use-package shr
  :custom
  (shr-use-xwidgets-for-media nil))

;;; eww.el ends here
