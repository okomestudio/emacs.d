;;; greppu.el --- greppu  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Greppu mode.
;;
;;; Code:

(require 'magit-todos)

(defcustom greppu-keywords '(("." . ("TODO" "WIP")))
  "Keywords used for scanning."
  :type 'list)

(defun greppu--search-regexp-pcre (search-regexp-pcre)
  "Intercept the generation of SEARCH-REGEXP-PCRE for `rg greppu'."
  (format ".*(%s).*" (string-join magit-todos-keywords-list "|")))

(magit-todos-defscanner
  "rg greppu"
  :test (executable-find "rg")
  :directory-form (if (equal directory default-directory)
                      nil
                    (f-relative directory default-directory))
  :allow-exit-codes (0 1)
  :command (let* ((search-regexp-pcre (greppu--search-regexp-pcre search-regexp-pcre))
                  (command (list "rg" "--no-heading" "--line-number"
                                 (when depth
                                   (list "--maxdepth" (1+ depth)))
                                 (when magit-todos-ignore-case
                                   "--ignore-case")
                                 (when magit-todos-exclude-globs
                                   (--map (list "--glob" (concat "!" it))
                                          magit-todos-exclude-globs))
                                 (unless magit-todos-submodule-list
                                   (--map (list "--glob" (concat "!" it))
                                          (magit-list-module-paths)))
                                 extra-args search-regexp-pcre directory)))
             command))

(define-derived-mode greppu-mode magit-todos-list-mode "greppu"
  "Major mode for displaying grep results."

  ;; TODO: Don't inherit magit keymap; disable git-releated actions for
  ;; safety.

  (hack-dir-local-variables-non-file-buffer))

;;;###autoload
(defun greppu-scan ()
  "Scan and open `greppu' buffer."
  (interactive)
  ;; TODO: magit-todos does not work outside a Git repo (checked with
  ;; `magit--not-inside-repository-error'. Make it work?
  (let* ((project-name (or (and (project-current)
                                (project-name (project-current)))
                           "nil"))
         (buffer (get-buffer-create (format "*GREPPU %s*" project-name))))
    (let ((inhibit-read-only t))
      (with-current-buffer buffer
        (erase-buffer)
        (greppu-mode)
        (make-local-variable 'magit-todos-keywords)
        (let ((keywords (or (cdr (assoc project-name greppu-keywords))
                            (cdr (assoc "." greppu-keywords)))))
          (when keywords
            (customize-set-variable 'magit-todos-keywords keywords)))

        (setq-local magit-todos-insert-after '(greppu))
        (setq-local magit-todos-scanner #'magit-todos--scan-with-rg-greppu)

        (magit-insert-section (greppu)
          (insert (format "GREPPU in project '%s':\n\n" project-name)))

        ;; TODO: Replace "TODOs" heading for the section.
        (magit-todos--insert-todos)))
    (display-buffer buffer)))

(provide 'greppu)
;;; greppu.el ends here
