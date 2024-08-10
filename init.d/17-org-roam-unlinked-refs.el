;;; 17-org-roam-unlinked-refs.el --- Org Roam Unlinked Refs  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; This is a module to enhance the unlinked references section of Org
;; Roam buffer.
;;
;;; Code:

;;; OVERRIDE
;;
;; The implementation within the following form has been submitted as
;; the following pull request:
;;
;;   - https://github.com/org-roam/org-roam/pull/2417/
;;
;; Here, we advise the entire `org-roam-unlinked-references-section'
;; function to add enhancements in order to avoid pinning the feature
;; branch for the local org-roam installation.
;;
(with-eval-after-load 'org-roam-mode
  (defcustom ok-org-roam-unlinked-references-word-boundary-re "|(\\b%1$s\\b)"
    "The word bounday regex used by ripgrep for unlinked references.
In such languages as CJK, the regex's word boundary (\b) does not
correctly determine how words and phrases should be tokenized.
This custom variable allows users to extend regex in those cases."
    :group 'org-roam
    :type 'string)

  (defcustom ok-org-roam-unlinked-references-max-results-count 1000
    "The max number of items in the unlinked references section.
Rendering of the unlinked references section can appear to freeze
when the match count is very large. This number limits the
maximum number of matched unlinked references to show to prevent
the issue."
    :group 'org-roam
    :type 'integer)

  (defun ok-org-roam-unlinked-references-result-filter-p (matched-text matched-file row col titles node)
    "Filter if the match is considered an unlinked reference.
Return non-nil if MATCHED-TEXT at ROW and COL in MATCHED-FILE is
an unlinked reference, or return nil. TITLES and NODE are
supplied for use in the conditional expression."
    (and (not (file-equal-p (org-roam-node-file node) matched-file))
         (member (downcase matched-text) (mapcar #'downcase titles))))

  (defun ok-org-roam-unlinked-references-preview-line (file row col file-prev row-prev col-prev)
    "Return the preview line from FILE.
The line was matched with text at ROW and COL. FILE-PREV,
ROW-PREV, and COL-PREV points to the previous line and can be
used to control rendering."
    (with-temp-buffer
      (insert-file-contents file)
      (forward-line (1- row))
      (buffer-substring-no-properties
       (save-excursion
         (beginning-of-line)
         (point))
       (save-excursion
         (end-of-line)
         (point)))))

  (defun ok-org-roam-unlinked-references-title-regex (titles)
    "Construct a ripgrep regex pattern from TITLES.
The output expression should be sanitized for the shell use."
    (format "'\\[([^[]]++|(?R))+\\]%s'"
            (mapconcat 'ok-org-roam-unlinked-references-apply-word-boundary-re titles "")))

  (defun ok-org-roam-unlinked-references-apply-word-boundary-re (title)
    "Wrap TITLE with word boundary regex.
The output expression should be sanitized for the shell use."
    (format ok-org-roam-unlinked-references-word-boundary-re
            (mapconcat #'shell-quote-argument
                       (split-string title "'")
                       "'\"'\"'")))

  (defun ok-org-roam-unlinked-references-file-glob-args ()
    "Construct file glob arguments for ripgrep."
    (mapconcat (lambda (glob) (concat "-g " glob))
               (org-roam--list-files-search-globs org-roam-file-extensions)
               " "))

  (defun ok-org-roam-unlinked-references-section (node)
    "The unlinked references section for NODE.
References from FILE are excluded."
    (when (and (executable-find "rg")
               (org-roam-node-title node)
               (not (string-match "PCRE2 is not available"
                                  (shell-command-to-string "rg --pcre2-version"))))
      (let* ((titles (cons (org-roam-node-title node)
                           (org-roam-node-aliases node)))
             (rg-command (concat "rg -L -o --vimgrep -P -i "
                                 (ok-org-roam-unlinked-references-file-glob-args)
                                 " "
                                 (ok-org-roam-unlinked-references-title-regex titles)
                                 " "
                                 org-roam-directory))
             (results (split-string (shell-command-to-string rg-command) "\n"))
             (match_count 0)
             f f-prev row row-prev col col-prev matched-text)
        (magit-insert-section (unlinked-references)
          (magit-insert-heading "Unlinked References:")
          (catch 'limit-result
            (dolist (line results)
              (save-match-data
                (when (string-match org-roam-unlinked-references-result-re line)
                  (setq f (match-string 1 line)
                        row (string-to-number (match-string 2 line))
                        col (string-to-number (match-string 3 line))
                        matched-text (match-string 4 line))
                  (when (and matched-text
                             (ok-org-roam-unlinked-references-result-filter-p
                              matched-text f row col titles node))
                    (magit-insert-section
                        section (org-roam-grep-section)
                        (oset section file f)
                        (oset section row row)
                        (oset section col col)
                        (insert (propertize
                                 (format "%s:%s:%s"
                                         (truncate-string-to-width (file-name-base f)
                                                                   15 nil nil t)
                                         row col)
                                 'font-lock-face 'org-roam-dim)
                                " "
                                (org-roam-fontify-like-in-org-mode
                                 (ok-org-roam-unlinked-references-preview-line
                                  f row col f-prev row-prev col-prev))
                                "\n")
                        (setq f-prev f
                              row-prev row
                              col-prev col
                              match_count (+ match_count 1))
                        (if (= match_count ok-org-roam-unlinked-references-max-results-count)
                            (insert (format "WARNING: Results truncated to %d items (%d potential matches)"
                                            match_count (length results))))))))
              (if (= match_count ok-org-roam-unlinked-references-max-results-count)
                  ;; Throw outside of magit-insert-section to render correct item count.
                  (throw 'limit-result match_count))))
          (insert ?\n)))))

  (advice-add #'org-roam-unlinked-references-section
              :override #'ok-org-roam-unlinked-references-section))


;;; CUSTOMIZATION
;;
;; Customize the unlinked references section for use with Japanese.
;;
(with-eval-after-load 'org-roam-mode
  ;; WORD BOUNDARY
  (let* (;; Strict version:
         ;;
         ;; Unlike English, Japanese words are not delimited by character
         ;; word boundary. In this stricter version, we rely on "joshi"
         ;; particles (助詞; ja.wikipedia.org/wiki/%E5%8A%A9%E8%A9%9E) as
         ;; boundary indicator. This implicitly assumes titles (words to be
         ;; delimited by the word boundary) to be "meishi" (名詞).

         ;; The collection of joshi that comes on the right side of word:
         (joshi_r '("か" "が" "かしら" "がてら" "から" "きり" "くらい" "ぐらい" "こそ"
                    "さ" "さえ" "しか" "ずつ"
                    "だけ" "だの" "で" "では" "でも" "と" "とは" "とも"
                    "ながら" "なぞ" "など" "なり" "なんぞ" "に" "ね" "の" "のみ"
                    "は" "ばかり" "へ" "ほど"
                    "まで" "も"
                    "や" "やら" "よ" "より"
                    "を"))
         ;; The collection of joshi that comes on the left side of word:
         (joshi_l (append joshi_r
                          '("かい" "かり" "けど" "けれど" "けれども"
                            "し" "ぜ" "ぞ"
                            "たり" "つつ" "ってば" "て" "ても" "ところで" "とも"
                            "な" "ので" "のに"
                            "ば"
                            "まま" "ものか" "ものの" "もん"
                            "わ")))

         (word-boundary-re-strict
          (concat "|(\\b%1$s\\b"
                  "|(?<="
                  (s-join "|" joshi_l) ")%1$s(?=" (s-join "|" joshi_r) "))"))

         ;; Lenient version:
         ;;
         ;; Use any Japanese characters as word boundary:
         (word-boundary-re-lenient
          (concat "|(\\b%1$s\\b"
                  "|(\\b%1$s\\b"
                  "|(?<=[^\x20-\x7e\xff61-\xff9f])%1$s(?=[^\x20-\x7e\xff61-\xff9f]))")))
    (setopt ok-org-roam-unlinked-references-word-boundary-re word-boundary-re-strict))

  ;; TITLE EXTRACTION REGEX
  (advice-add
   'ok-org-roam-unlinked-references-title-regex
   :override
   (lambda (titles)
     (let ((bounded-re (substring (mapconcat
                                   #'ok-org-roam-unlinked-references-apply-word-boundary-re
                                   titles "")
                                  1))

           ;; For variable-lengths lookbehinds in PCRE, see:
           ;;
           ;;   - www.drregex.com/2019/02/variable-length-lookbehinds-actually.html
           ;;
           ;; `plb' is for positive lookbehind; `nlb' is for negative lookbehind.
           (plb "(?=(?'a'[\\s\\S]*))(?'b'(%s)(?=\\k'a'\\z)|(?<=(?=x^|(?&b))[\\s\\S]))")
           (nlb "(?!(?=(?<a>[\\s\\S]*))(?<b>(%s)(?=\\k<a>\\z)|(?<=(?=x^|(?&b))[\\s\\S])))")

           ;; The list of substrings for negative matching:
           (lines-to-ignore '("begin_src +"
                              "filetags:( [-_0-9A-Za-z]+)* "
                              "header-args:"
                              "PYTHONDONTWRITEBYTECODE=1 "
                              "transclude:")))
       (format "'\\[\\[id:[0-9a-f-]+\\]\\[[^][]*(%s)[^][]*\\]\\]|%s(%s)'"
               bounded-re
               (format nlb (string-join lines-to-ignore "|"))
               bounded-re))))

  ;; TITLE TRANSFORMATION AND SANITIZATION
  (advice-add
   'ok-org-roam-unlinked-references-apply-word-boundary-re
   :override
   (lambda (title)
     (let* (;; Expand quote variants:
            (s (replace-regexp-in-string " ['\‘]\\(\\w\\)" " ['\‘]\\1" title))
            (s (replace-regexp-in-string "\\(\\w\\)['\’]" "\\1['\’]" s))
            (s (replace-regexp-in-string " [\"\“]\\(\\w\\)" " [\"\“]\\1" s))
            (s (replace-regexp-in-string "\\(\\w\\)[\"\”]" "\\1[\"\”]" s))

            ;; Apply the original sanitization:
            (s (format ok-org-roam-unlinked-references-word-boundary-re
                       (mapconcat #'shell-quote-argument
                                  (split-string s "'")
                                  "'\"'\"'")))

            ;; Some special chars needs unescaping after `shell-quotes':
            (s (replace-regexp-in-string "[\\][[]\\([^][]+\\)[\\][]]" "[\\1]" s)))
       s)))

  ;; PREDICATE FOR UNLINKED REFERENCE CHECK
  (advice-add
   'ok-org-roam-unlinked-references-result-filter-p
   :override
   (lambda (matched-text matched-file row col titles node)
     (let ((linked-re (format "\\[\\[id:%s\\]\\[.*\\]\\]" (org-roam-node-id node))))
       (if (not (file-equal-p (org-roam-node-file node) matched-file))
           ;; The matched text is in a different file from the current node.
           (not (string-match linked-re matched-text)) ;; Is this reference unlinked?
         ;; The matched text is in the same file of the current node.
         (let* ((other-node (save-match-data
                              (with-current-buffer (find-file-noselect matched-file)
                                (save-excursion
                                  (goto-char (point-min))
                                  (forward-line (1- row))
                                  (move-to-column col)
                                  (org-roam-node-at-point))))))
           (if (not (string-equal (org-roam-node-id node)
                                  (org-roam-node-id other-node)))
               ;; The matched text is in a different node from the
               ;; current node within the same file.
               (not (string-match linked-re matched-text)) ;; Is this reference unlinked?
             ))))))

  ;; PREVIEW LINE RENDERING
  (advice-add
   'ok-org-roam-unlinked-references-preview-line
   :around
   (lambda (orig-fun file row col file-prev row-prev col-prev)
     (if (and (string= file file-prev) (= row row-prev))
         ;; Use a "ditto" mark if the current line is
         ;; similar to the previous line.
         "⎯〃⎯"
       (funcall orig-fun file row col file-prev row-prev col-prev)))))

;; Local Variables:
;; nameless-aliases: (("" . "ok-org-roam"))
;; End:
;;; 17-org-roam-unlinked-refs.el ends here
