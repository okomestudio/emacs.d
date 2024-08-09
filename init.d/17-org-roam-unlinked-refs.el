;;; 17-org-roam-unlinked-refs.el --- Org Roam Unlinked Refs  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; This is a module to enhance the unlinked references section of Org
;; Roam buffer. This implementation has been submitted as the
;; following pull request:
;;
;; - https://github.com/org-roam/org-roam/pull/2417/
;;
;; Here, we advise the entire `org-roam-unlinked-references-section'
;; function to add enhancements in order to avoid pinning local
;; org-roam installation to the feature branch.
;;
;;; Code:

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
    (format "\"\\[([^[]]++|(?R))+\\]%s\""
            (mapconcat 'ok-org-roam-unlinked-references-apply-word-boundary-re titles "")))

  (defun ok-org-roam-unlinked-references-apply-word-boundary-re (title)
    "Wrap TITLE with word boundary regex.
The output expression should be sanitized for the shell use."
    (format ok-org-roam-unlinked-references-word-boundary-re (shell-quote-argument title)))

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
                             (org-roam-unlinked-references-result-filter-p matched-text f row col titles node))
                    (magit-insert-section section (org-roam-grep-section)
                                          (oset section file f)
                                          (oset section row row)
                                          (oset section col col)
                                          (insert (propertize (format "%s:%s:%s"
                                                                      (truncate-string-to-width (file-name-base f) 15 nil nil t)
                                                                      row col) 'font-lock-face 'org-roam-dim)
                                                  " "
                                                  (org-roam-fontify-like-in-org-mode
                                                   (ok-org-roam-unlinked-references-preview-line f row col f-prev row-prev col-prev))
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
  )

;; Local Variables:
;; nameless-aliases: (("" . "ok-org-roam"))
;; End:
;;; 17-org-roam-unlinked-refs.el ends here
