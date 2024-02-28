;;; greppu.el --- greppu  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Greppu mode.
;;
;;; Code:

(require 'magit-todos)

(defcustom greppu-keywords '(("." . hl-todo-keyword-faces))
  "Keywords used for scanning.
Ether a list of keyword string or a list of cons (keyword . color).")

(defcustom greppu-exclude-globs '(".git/")
  "Glob patterns to exclude from searches."
  :type '(repeat string))

(defcustom greppu-search-regexp-pcre #'identity
  "Function to modify PCRE regexp for search.")

;; functions

(defun greppu-search-regexp-pcre-simple (search-regexp-pcre)
  "Intercept the generation of SEARCH-REGEXP-PCRE for `rg greppu'."
  (format ".*(%s).*" (string-join magit-todos-keywords "|")))

(magit-todos-defscanner
  "rg greppu"
  :test (executable-find "rg")
  :directory-form (if (equal directory default-directory)
                      nil
                    (f-relative directory default-directory))
  :allow-exit-codes (0 1)
  :command (let* ((search-regexp-pcre (funcall greppu-search-regexp-pcre search-regexp-pcre))
                  (command (list "rg" "--no-heading" "--line-number"
                                 (when depth
                                   (list "--maxdepth" (1+ depth)))
                                 (when magit-todos-ignore-case
                                   "--ignore-case")
                                 (when magit-todos-exclude-globs
                                   (--map (list "--glob" (concat "!" it))
                                          greppu-exclude-globs))
                                 (unless magit-todos-submodule-list
                                   (--map (list "--glob" (concat "!" it))
                                          (magit-list-module-paths)))
                                 extra-args search-regexp-pcre directory)))
             command))

(define-derived-mode greppu-mode magit-status-mode "greppu"
  "Major mode for displaying grep results."
  :interactive nil

  ;; TODO: Don't inherit magit keymap; disable unsafe git-releated
  ;; actions for safety.
  (hack-dir-local-variables-non-file-buffer)

  (make-local-variable 'hl-todo-keyword-faces)
  (make-local-variable 'magit-todos-insert-after)
  (make-local-variable 'magit-todos-keywords)
  (make-local-variable 'magit-todos-scanner)
  (setq-local magit-todos-insert-after
              (customize-set-variable 'magit-todos-insert-after '(greppu)))
  (setq-local magit-todos-scanner
              (customize-set-variable 'magit-todos-scanner
                                      #'magit-todos--scan-with-rg-greppu))

  (let* ((project-name (or (and (project-current)
                                (project-name (project-current)))
                           "."))
         (keywords (assoc-default project-name greppu-keywords)))
    (when keywords
      (setq-local hl-todo-keyword-faces
                  (customize-set-variable 'hl-todo-keyword-faces keywords))
      (setq-local magit-todos-keywords
                  (customize-set-variable 'magit-todos-keywords
                                          (mapcar (lambda (it)
                                                    (cl-typecase it
                                                      (string it)
                                                      (cons (car it))))
                                                  keywords))))
    (setq-local magit-todos-section-heading
                (format "GREPPU in '%s'" project-name))

    (erase-buffer)
    (magit-insert-section (greppu))
    (magit-todos--insert-todos)))

;;;###autoload
(defun greppu-scan ()
  "Scan and open `greppu' buffer."
  (interactive)
  ;; TODO: magit-todos does not work outside a Git repo (checked with
  ;; `magit--not-inside-repository-error'. Make it work?
  (let* ((project-name (or (and (project-current)
                                (project-name (project-current)))
                           "."))
         (buffer (get-buffer-create (format "*GREPPU %s*" project-name))))
    (let ((inhibit-read-only t))
      (with-current-buffer buffer
        (greppu-mode)))
    (display-buffer buffer)))

(provide 'greppu)
;;; greppu.el ends here
