;;; 80-eww.el --- EWW  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Emacs's builtin web browser.
;;
;;; Code:

(use-package eww
  :config
  (require 'okutil)

  (defun init-eww--eww-set-start-at (url-regexp search-regexp)
    "When site matches URL-REGEXP, start displaying from line matching SEARCH-REGEXP.

See http://emacs.rubikitch.com/eww-weblio/ for reference."
    (when (string-match url-regexp (plist-get eww-data :url))
      (goto-char (point-min))
      (when (re-search-forward search-regexp nil t)
        (recenter 0))))

  (defun init-eww--eww-render--after (&rest _)
    "Move to the specified line on page load."
    (init-eww--eww-set-start-at "amazon.co.jp" "^結果")
    (init-eww--eww-set-start-at "amazon.com" "^RESULTS")
    (init-eww--eww-set-start-at "b.hatena.ne.jp" "^記事へのコメント")
    (init-eww--eww-set-start-at "goodreads.com" "^Page ")
    (init-eww--eww-set-start-at "en.m.wikipedia.org" "^ *Search")
    (init-eww--eww-set-start-at "ja.m.wikipedia.org" "^ *検索")
    (init-eww--eww-set-start-at "www.weblio.jp" "^ *Weblio 辞書"))

  (add-hook 'eww-after-render-hook 'init-eww--eww-render--after)

  ;; Wrap eww to enable quicker look up in some sites.
  (defun init-eww--make-query (site-url str)
    "Look up term STR at SITE-URL in eww."
    (eww-browse-url (format site-url (url-hexify-string str))))

  (defun eww-search-amazon-en (str)
    (interactive (list (okutil-string-from-region-or-prompt "Amazon (US): ")))
    (init-eww--make-query "https://amazon.com/s?k=%s" str))

  (defun eww-search-amazon-ja (str)
    (interactive (list (okutil-string-from-region-or-prompt "Amazon (JP): ")))
    (init-eww--make-query "https://amazon.co.jp/s?k=%s" str))

  (defun eww-search-duckduckgo-en (str)
    (interactive (list (okutil-string-from-region-or-prompt "DuckDuckGo (en): ")))
    (init-eww--make-query "https://html.duckduckgo.com/html/?q=%s&kp=-2&kl=wt-wt&ks=s" str))

  (defun eww-search-duckduckgo-ja (str)
    (interactive (list (okutil-string-from-region-or-prompt "DuckDuckGo (ja): ")))
    (init-eww--make-query "https://html.duckduckgo.com/html/?q=%s&kp=-2&kl=jp-jp&ks=s" str))

  (defun eww-search-goodreads (str)
    (interactive (list (okutil-string-from-region-or-prompt "Goodreads: ")))
    (init-eww--make-query "https://goodreads.com/search?q=%s" str))

  (defun eww-search-weblio (str)
    (interactive (list (okutil-string-from-region-or-prompt "Weblio: ")))
    (init-eww--make-query "https://www.weblio.jp/content/%s" (upcase str)))

  (defun eww-search-wikipedia-en (str)
    (interactive (list (okutil-string-from-region-or-prompt "Wikipedia (en): ")))
    (init-eww--make-query "https://en.m.wikipedia.org/wiki/%s" str))

  (defun eww-search-wikipedia-ja (str)
    (interactive (list (okutil-string-from-region-or-prompt "Wikipedia (ja): ")))
    (init-eww--make-query "https://ja.m.wikipedia.org/wiki/%s" str))

  (defun eww-search-justapedia-en (str)
    (interactive (list (okutil-string-from-region-or-prompt "Justapedia (en): ")))
    (init-eww--make-query "https://justapedia.org/wiki/%s" str)))


(use-package shr
  :custom
  (shr-use-xwidgets-for-media nil))

;;; 80-eww.el ends here
