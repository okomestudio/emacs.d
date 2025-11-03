;;; maj-eww.el --- EWW  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the EWW major mode.
;;
;;; Code:

(use-package eww
  ;; Emacs Web Wowser is a web browser without CSS or JavaScript.

  ;; TODO(2025-11-01): Make `eww-retrieve-command' switchable, at least to nil
  ;; so that it falls back to `url-retrieve'.
  :custom (eww-retrieve-command '("readable"))
  :ensure-system-package (readable . "npm install -g readability-cli")
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

  ;; Consolidated Web Search UX

  (defcustom eww-search-web-sites
    '(("Amazon (JP)" . "https://amazon.co.jp/s?k=%s")
      ("Amazon (US)" .  "https://amazon.com/s?k=%s")
      ("DuckDuckGo (en)" . "https://html.duckduckgo.com/html/?q=%s&kp=-2&kl=wt-wt&ks=s")
      ("DuckDuckGo (ja)" . "https://html.duckduckgo.com/html/?q=%s&kp=-2&kl=jp-jp&ks=s")
      ("Goodreads" . "https://goodreads.com/search?q=%s")
      ("Justapedia (en)" . "https://justapedia.org/wiki/%s")
      ("Weblio" . "https://www.weblio.jp/content/%s")
      ("Wikipedia (en)" . "https://en.wikipedia.org/wiki/%s")
      ("Wikipedia (ja)" . "https://ja.wikipedia.org/wiki/%s"))
    "Alist of web search engines.")

  (defun eww-search-web (term &optional engine)
    "Perfom web search for TERM using a search ENGINE.
ENGINE is a key in the alist `eww-search-web-sites'."
    (interactive
     (list
      (if-let*
          ((w (or (and (region-active-p)
                       (prog1
                           (buffer-substring-no-properties
                            (region-beginning) (region-end))
                         (deactivate-mark)))
                  (thing-at-point 'word 'no-properties))))
          w
        (read-string "Search term: "))
      nil))
    (let* ((key (or engine
                    (completing-read "Search engine: "
                                     (--map (car it) eww-search-web-sites)
                                     nil t)))
           (url-tmpl (alist-get key eww-search-web-sites nil nil #'equal))
           (url (format url-tmpl (url-hexify-string term))))
      (eww-browse-url url))))

(use-package shr
  :custom (shr-use-xwidgets-for-media nil))

(provide 'maj-eww)
;;; maj-eww.el ends here
