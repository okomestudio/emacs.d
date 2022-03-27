;;; init-file-type-modes.el --- File type modes  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; INI

(use-package any-ini-mode
  :ensure nil
  :init (ensure-file-from-url "https://www.emacswiki.org/emacs/download/any-ini-mode.el")
  :mode "\\.ini\\'" "\\.conf\\'")


;; JSON

(use-package json-mode
  :after (web-beautify)
  :bind (:map json-mode-map ("C-c b" . ts/beautify-json-via-python))
  :custom (js-indent-level 4)
  :mode "\\.json\\(\\.j2\\)?\\'"
  :config
  (defun ts/beautify-json-via-python ()
    "See, e.g., https://emacs.stackexchange.com/a/12152/599"
    (interactive)
    (save-excursion
      (shell-command-on-region (point-min) (point-max)
                               "python -m json.tool"
                               (buffer-name) t)))

  (defun ts/beautify-json-via-js ()
    (interactive)
    (save-excursion
      (let ((web-beautify-args '("-f" "-"
                                 "--end-with-newline"
                                 "--keep-array-indentation"
                                 "--no-preserve-newlines"
                                 "--jslint-happy"
                                 "--wrap-line-length" "88")))
        (web-beautify-js)))))

(use-package jq-mode
  :ensure-system-package (jq . "sudo apt install jq"))


;; MARKDOWN

;; This mode allows browser preview with C-c C-c v
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :ensure-system-package ((pandoc . "sudo apt install pandoc"))
  :init (setq markdown-command "pandoc")
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.lr\\'" . markdown-mode)))


;; RST

(use-package rst-mode
  :ensure nil
  :ensure-system-package ((sphinx-quickstart . "pip install sphinx"))
  :mode "\\.rst\\'")


;; SYSTEMD

(use-package systemd
  :mode
  ("\\.\\(service|timer\\)\\'" . systemd-mode))


;; YAML

(use-package yaml-mode
  :hook (yaml-mode . (lambda () (typo-mode -1)))
  :mode ("\\.ya?ml\\(\\.j2\\)?\\'" . yaml-mode))


(provide 'init-file-type-modes)
;;; init-file-type-modes.el ends here
