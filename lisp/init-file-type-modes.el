;;; init-file-type-modes.el --- File type modes  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; INIT

(use-package any-ini-mode
  :ensure nil
  :init (ensure-file-from-url "https://www.emacswiki.org/emacs/download/any-ini-mode.el")
  :mode ".*\\.ini$" ".*\\.conf$" ".*\\.service$")


;; JSON

(use-package json-mode
  :after (web-beautify)
  :bind (:map json-mode-map ("C-c b" . ts/beautify-json-via-python))
  :custom (js-indent-level 4)
  :mode ("\\.json\\'" "\\.json.j2\\'")
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
  :hook ((markdown-mode) . remove-trailing-whitespaces-on-save)
  :init (setq markdown-command "pandoc")
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.lr\\'" . markdown-mode)))


;; RST

(use-package rst-mode
  :ensure nil
  :hook ((rst-mode . remove-trailing-whitespaces-on-save)))


;; YAML

(use-package yaml-mode
  :hook
  ((yaml-mode . (lambda () (typo-mode -1)))
   (yaml-mode . remove-trailing-whitespaces-on-save))

  :mode "\\.ya?ml\\'" "\\.ya?ml.j2\\'")

(provide 'init-file-type-modes)
;;; init-file-type-modes.el ends here
