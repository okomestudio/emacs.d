;;; 10-org-roam.el --- Org Roam  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; A tool for networked thought. It reproduces some of Roam Research's key
;; features within Org.
;;
;;; Code:

(use-package org-roam
  :after (org)

  :straight
  (;; pull from fork for custom features
   :type git
   :host github
   :repo "okomestudio/org-roam"
   :branch "okome"
   :fork "okomestudio")

  :bind
  (("C-c n c" . (lambda () (interactive) (org-capture nil "f")))
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n l" . org-roam-buffer-toggle)

   :map org-mode-map
   ("C-M-i" . completion-at-point))

  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)

  :custom
  (org-roam-completion-everywhere nil)
  (org-roam-dailies-capture-templates '(("d" "default" entry "* %?\n<%<%Y-%m-%d %a %H:%M>>"
                                         :target
                                         (file+head "%<%Y-%m-%d>.org"
                                                    "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-dailies-directory "journal/")
  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-db-location (file-truename "~/.config/emacs/roam/.roam.db"))
  (org-roam-directory (file-truename "~/.config/emacs/roam"))
  (org-roam-extract-new-file-path "topic/${id}/${slug}.org")
  (org-roam-mode-sections (list #'org-roam-backlinks-section
                                #'org-roam-reflinks-section
                                #'org-roam-unlinked-references-section))
  (org-roam-node-display-template (concat "​​​​​${my-node-entry:*}"
                                          (propertize "${tags:16}" 'face 'org-tag)
                                          " ${my-node-timestamp:*}"))

  :preface
  (put 'orb-preformat-keywords 'safe-local-variable #'listp)
  (put 'org-roam-capture-templates 'safe-local-variable #'listp)
  (put 'org-roam-dailies-capture-templates 'safe-local-variable #'listp)
  (put 'org-roam-db-location 'safe-local-variable #'stringp)
  (put 'org-roam-directory 'safe-local-variable #'stringp)
  (put 'org-roam-mode-sections 'safe-local-variable #'listp)
  (put 'org-roam-ui-port 'safe-local-variable #'integerp)

  :init
  (with-eval-after-load 'org-roam-node
    (cl-defmethod org-roam-node-my-node-timestamp ((node org-roam-node))
      (marginalia--time
       (let* ((node-properties (org-roam-node-properties node))
              (node-mtime (cdr (assoc "MTIME" node-properties)))
              (inhibit-message t))
         (if node-mtime
             (org-roam-timestamps-encode (car (split-string node-mtime)))
           (org-roam-node-file-mtime node)))))


    (defun init-org--get-node-id-from-file (file)
      (caar (org-roam-db-query `[:select nodes:id :from nodes :where (and (= nodes:file ,file) (= nodes:level 0))])))

    (setq init-org--file-node-cache '())

    (defun init-org--get-node-from-file (file)
      (let ((x (assoc file init-org--file-node-cache)))
        (if x
            (let* ((ttl 30.0)
                   (v (cdr x))
                   (tt (car v))
                   (dt (- (float-time) tt)))
              (if (< dt ttl)
                  (car (cdr v))
                (setf init-org--file-node-cache (assoc-delete-all file init-org--file-node-cache))
                (init-org--get-node-from-file file)))
          (let ((y (org-roam-node-from-id (init-org--get-node-id-from-file file))))
            (add-to-list 'init-org--file-node-cache `(,file ,(float-time) ,y))
            y))))

    (defun init-org--get-parent-title (node)
      (let ((parent (cdr (assoc-string "PARENT" (org-roam-node-properties node)))))
        (when parent
          (replace-regexp-in-string "\\[\\[\\(.+\\)\\]\\[\\([[:space:]0-9a-zA-Zぁ-んァ-ヶｱ-ﾝﾞﾟ一-龠]+\\)\\]\\]"
                                    "\\2"
                                    parent))))

    (cl-defmethod org-roam-node-my-node-entry ((node org-roam-node))
      (let* ((title-annotate-color "SeaGreen4")
             (node-title (org-roam-node-title node))
             (node-file-title (or (if (not (s-blank? (org-roam-node-file-title node)))
                                      (org-roam-node-file-title node))
                                  (file-name-nondirectory (org-roam-node-file node))))
             (title-aux (if (string= node-title node-file-title)
                            (let ((x (init-org--get-parent-title node)))
                              (if x (list " ❬ " x)))
                          (if (member node-title (org-roam-node-aliases node))
                              (list " = " node-file-title)
                            (let ((x (init-org--get-parent-title (init-org--get-node-from-file (org-roam-node-file node)))))
                              (if x (list " ❬ " x) (list " ❬ " node-file-title)))
                            ))))
        (concat
         node-title
         (if (not title-aux)
             ""
           (let ((sym (nth 0 title-aux))
                 (aux (nth 1 title-aux)))
             (concat
              (propertize sym 'face `(:foreground ,title-annotate-color))
              (propertize aux 'face `(:foreground ,title-annotate-color :slant italic))))))))

    (defun init-org-roam--org-roam-node-slug (title)
      (let* (;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
             (slug-trim-chars '(768   ; U+0300 COMBINING GRAVE ACCENT
                                769   ; U+0301 COMBINING ACUTE ACCENT
                                770   ; U+0302 COMBINING CIRCUMFLEX ACCENT
                                771   ; U+0303 COMBINING TILDE
                                772   ; U+0304 COMBINING MACRON
                                774   ; U+0306 COMBINING BREVE
                                775   ; U+0307 COMBINING DOT ABOVE
                                776   ; U+0308 COMBINING DIAERESIS
                                777   ; U+0309 COMBINING HOOK ABOVE
                                778   ; U+030A COMBINING RING ABOVE
                                779   ; U+030B COMBINING DOUBLE ACUTE ACCENT
                                780   ; U+030C COMBINING CARON
                                795   ; U+031B COMBINING HORN
                                803   ; U+0323 COMBINING DOT BELOW
                                804   ; U+0324 COMBINING DIAERESIS BELOW
                                805   ; U+0325 COMBINING RING BELOW
                                807   ; U+0327 COMBINING CEDILLA
                                813   ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                                814   ; U+032E COMBINING BREVE BELOW
                                816   ; U+0330 COMBINING TILDE BELOW
                                817)))  ; U+0331 COMBINING MACRON BELOW
        (cl-flet* ((nonspacing-mark-p (char)
                     (memq char slug-trim-chars))
                   (strip-nonspacing-marks (s)
                     (string-glyph-compose
                      (apply #'string
                             (seq-remove #'nonspacing-mark-p
                                         (string-glyph-decompose s)))))
                   (cl-replace (title pair)
                     (replace-regexp-in-string (car pair) (cdr pair) title)))
          (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-") ;; convert anything not alphanumeric
                          ("--*" . "-") ;; remove sequential underscores
                          ("^-" . "")   ;; remove starting underscore
                          ("-$" . ""))) ;; remove ending underscore
                 (slug (-reduce-from #'cl-replace
                                     (strip-nonspacing-marks title)
                                     pairs)))
            (downcase slug)))))

    (cl-defmethod org-roam-node-slug ((node org-roam-node))
      "Return the slug of NODE. Overridden to use hyphens instead of underscores."
      (init-org-roam--org-roam-node-slug (org-roam-node-title node))))

  :config
  (setq find-file-visit-truename t) ;; See 5.3 Setting up Org-roam

  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode)

  ;; Customized unlinked references section

  ;; Stricter:
  ;; 助詞 (https://ja.wikipedia.org/wiki/%E5%8A%A9%E8%A9%9E)
  (setq joshi_r '(;; タイトルを名詞と前提
                  "か" "が" "かしら" "がてら" "から" "きり" "くらい" "ぐらい" "こそ"
                  "さ" "さえ" "しか" "ずつ"
                  "だけ" "だの" "で" "では" "でも" "と" "とは" "とも"
                  "ながら" "なぞ" "など" "なり" "なんぞ" "に" "ね" "の" "のみ"
                  "は" "ばかり" "へ" "ほど"
                  "まで" "も"
                  "や" "やら" "よ" "より"
                  "を"))
  (setq joshi_l (append joshi_r
                        '("かい" "かり" "けど" "けれど" "けれども"
                          "し" "ぜ" "ぞ"
                          "たり" "つつ" "ってば" "て" "ても" "ところで" "とも"
                          "な" "ので" "のに"
                          "ば"
                          "まま" "ものか" "ものの" "もん"
                          "わ")))
  (setq org-roam-unlinked-references-word-boundary-re
        (concat "|(\\b%1$s\\b"
                "|(?<=" (s-join "|" joshi_l) ")%1$s(?=" (s-join "|" joshi_r) "))"))
  ;; Lenient version:
  ;;   "|(\\b%1$s\\b|(?<=[^\x20-\x7e\xff61-\xff9f])%1$s(?=[^\x20-\x7e\xff61-\xff9f]))"

  (defun init-org-roam--title-regex (orig-fun titles)
    (let ((bounded-re (substring (mapconcat #'org-roam-unlinked-references-apply-word-boundary-re titles "") 1))
          ;; See http://www.drregex.com/2019/02/variable-length-lookbehinds-actually.html
          (positive-lookbehind "(?=(?'a'[\\s\\S]*))(?'b'(%s)(?=\\k'a'\\z)|(?<=(?=x^|(?&b))[\\s\\S]))")
          (negative-lookbehind "(?!(?=(?<a>[\\s\\S]*))(?<b>(%s)(?=\\k<a>\\z)|(?<=(?=x^|(?&b))[\\s\\S])))"))
      (format "\"\\[\\[id:[0-9a-f-]+\\]\\[[^][]*(%s)[^][]*\\]\\]|%s(%s)\""
              bounded-re
              (format negative-lookbehind
                      (mapconcat (lambda (s) s)
                                 '("begin_src +"
                                   "filetags:( [-_0-9A-Za-z]+)* "
                                   "header-args:"
                                   "PYTHONDONTWRITEBYTECODE=1 ")
                                 "|"))
              bounded-re)))

  (advice-add 'org-roam-unlinked-references-title-regex
              :around #'init-org-roam--title-regex)

  (defun init-org-roam--apply-word-boundary-re (orig-fun title)
    (let ((s title))
      ;; Expand and match quote variants:
      (setq s (replace-regexp-in-string " [\'\‘]\\(\\w\\)" " [\'\‘]\\1" s))
      (setq s (replace-regexp-in-string "\\(\\w\\)[\'\’]" "\\1[\'\’]" s))
      (setq s (replace-regexp-in-string " [\"\“]\\(\\w\\)" " [\"\“]\\1" s))
      (setq s (replace-regexp-in-string "\\(\\w\\)[\"\”]" "\\1[\"\”]" s))
      (let ((s (funcall orig-fun s)))
        ;; Since orig-fun shell-quotes special chars, some needs unescape:
        (setq s (replace-regexp-in-string "[\\][[]\\([^][]+\\)[\\][]]" "[\\1]" s))
        s)))

  (advice-add 'org-roam-unlinked-references-apply-word-boundary-re
              :around #'init-org-roam--apply-word-boundary-re)

  (defun init-org-roam--result-filter-p (orig-fun matched-text matched-file row col titles node)
    (let* ((linked-re (format "\\[\\[id:%s\\]\\[.*\\]\\]" (org-roam-node-id node)))
           result)
      (if (not (file-equal-p (org-roam-node-file node) matched-file))
          (setq result (not (string-match linked-re matched-text))) ;; Test if unlinked ref
        ;; Matched text within the same file, possibly of a different node
        (let* ((other-node (save-match-data
                             (with-current-buffer (find-file-noselect matched-file)
                               (save-excursion
                                 (goto-char (point-min))
                                 (forward-line (1- row))
                                 (move-to-column col)
                                 (org-roam-node-at-point))))))
          (if (not (string-equal (org-roam-node-id node)
                                 (org-roam-node-id other-node)))
              ;; Matched text within a different node within the same file
              (setq result (not (string-match linked-re matched-text))) ;; Text if unlinked ref
            )))
      result))

  (advice-add 'org-roam-unlinked-references-result-filter-p
              :around #'init-org-roam--result-filter-p)

  (defun init-org-roam--unlinked-references-preview-line (orig-fun file row col file-prev row-prev col-prev)
    "Use ellipsis for duplicate line."
    (if (and (string= file file-prev) (= row row-prev))
        "⎯〃⎯"
      (funcall orig-fun file row col file-prev row-prev col-prev)))

  (advice-add 'org-roam-unlinked-references-preview-line
              :around #'init-org-roam--unlinked-references-preview-line))


(use-package org-roam-ui
  :after org-roam

  :custom
  (org-roam-ui-follow t)
  (org-roam-ui-sync-theme t)
  (org-roam-ui-update-on-save t))


(use-package org-roam-bibtex
  :after org-roam

  :custom
  (orb-insert-link-description "${author-abbrev} ${date}")
  (orb-roam-ref-format 'org-ref-v3)

  :config
  (require 'org-ref)) ; optional: if using Org-ref v2 or v3 citation links


(use-package org-ref
  ;; For citations, cross-references, bibliographies.
  :custom
  (bibtex-completion-pdf-field "file")

  :preface
  (put 'bibtex-completion-bibliography 'safe-local-variable #'listp))


(use-package org-roam-timestamps
  :after org-roam

  :custom
  (org-roam-timestamps-minimum-gap 86400)
  (org-roam-timestamps-parent-file nil)
  (org-roam-timestamps-remember-timestamps t)

  :config
  (org-roam-timestamps-mode))


(use-package adaptive-wrap
  :hook
  (org-roam-mode . (lambda ()
                     (turn-on-visual-line-mode)
                     ;; Format org-roam buffer so that unlinked reference list
                     ;; are easier to see.
                     (setq-local adaptive-wrap-extra-indent 4)
                     (adaptive-wrap-prefix-mode +1))))

;;; 10-org-roam.el ends here
