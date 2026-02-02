;;; maj-eww.el --- Emacs Web Wowser (EWW)  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Emacs Web Wowser (EWW) is a web browser for Emacs.
;;
;;; Code:

(use-package eww
  ;; Emacs Web Wowser is a web browser without CSS or JavaScript.

  ;; TODO(2025-11-01): Make `eww-retrieve-command' switchable, at least to nil
  ;; so that it falls back to `url-retrieve'.
  :custom (eww-retrieve-command '("readable"))
  :ensure-system-package (readable . "npm install -g readability-cli")
  :config
  (defun ok-eww-reddit-redirect(url)
    "Redirect reddit.com to old.reddit.com automatically.
See reddit.com/r/emacs/comments/1bcf8v3."
    (replace-regexp-in-string "https://www.reddit.com"
                              "https://old.reddit.com"
                              url))

  (setopt eww-url-transformers '(eww-remove-tracking ok-eww-reddit-redirect))

  (defcustom ok-eww-browse-from
    '(("amazon.co.jp" . "^結果")
      ("amazon.com" . "^RESULTS")
      ("b.hatena.ne.jp" . "^記事へのコメント")
      ("goodreads.com" . "^Page ")
      ("en.m.wikipedia.org" . "^ *Search")
      ("ja.m.wikipedia.org" . "^ *検索")
      ("www.weblio.jp" . "^ *Weblio 辞書"))
    "Alist of cons `(URL-RE . SEARCH-RE)'."
    :type '(alist :key-type string :value-type string)
    :group 'eww)

  (defun ok-eww-browse-from (&rest _)
    "Move to specified line within web page.
When the site URL matches the regexp URL-RE, start displaying from line matching
the regexp SEARCH-RE. Define the regexp pairs in the custom variable
`ok-eww-browse-from'.

The implementation is inspired by http://emacs.rubikitch.com/eww-weblio/."
    (when-let* ((url (plist-get eww-data :url)))
      (pcase-dolist (`(,url-re . ,search-re) ok-eww-browse-from)
        (when (string-match url-re url)
          (goto-char (point-min))
          (when (re-search-forward search-re nil t)
            (recenter 0))))))

  (add-hook 'eww-after-render-hook 'ok-eww-browse-from)

  ;; Consolidated Web Search UX

  (defcustom ok-eww-search-web-sites
    '(("Amazon (JP)" . "https://www.amazon.co.jp/s?k=%s&language=ja_JP&currency=JPY")
      ("Amazon (US)" .  "https://www.amazon.com/s?k=%s&language=en_US&currency=USD")
      ("DuckDuckGo (en)"
       . "https://html.duckduckgo.com/html/?q=%s&kp=-2&kl=wt-wt&ks=s")
      ("DuckDuckGo (ja)"
       . "https://html.duckduckgo.com/html/?q=%s&kp=-2&kl=jp-jp&ks=s")
      ("Goodreads" . "https://goodreads.com/search?q=%s")
      ("Justapedia (en)" . "https://justapedia.org/wiki/%s")
      ("Stanford Encyclopedia of Philosophy"
       . "https://plato.stanford.edu/search/searcher.py?query=%s")
      ("Weblio" . "https://www.weblio.jp/content/%s")
      ("Wikipedia (en)" . "https://en.wikipedia.org/wiki/%s")
      ("Wikipedia (ja)" . "https://ja.wikipedia.org/wiki/%s"))
    "Web search query URLs."
    :type '(alist :key-type string :value-type string)
    :group 'eww)

  (defun ok-eww-search-web (term &optional engine)
    "Search for TERM using web search ENGINE.
ENGINE is a key in the alist `ok-eww-search-web-sites'."
    (interactive
     (list (let* ((init (or (and (region-active-p)
                                 (prog1
                                     (buffer-substring-no-properties
                                      (region-beginning) (region-end))
                                   ;; (deactivate-mark)
                                   ))
                            (thing-at-point 'word 'no-properties))))
             (read-string "Search term: " init nil nil t))
           (completing-read "Search engine: "
                            (mapcar #'car ok-eww-search-web-sites)
                            nil t)))
    (if-let* ((tmpl (alist-get engine ok-eww-search-web-sites nil nil #'equal))
              (url (format tmpl (url-hexify-string term))))
        (eww-browse-url url)
      (warn "Error constructing search query URL"))))

(use-package shr
  :custom (shr-use-xwidgets-for-media nil))

(provide 'maj-eww)
;;; maj-eww.el ends here
