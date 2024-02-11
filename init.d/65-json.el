;;; 65-json.el --- JSON  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package json-mode
  :after (web-beautify)

  :bind
  (;
   :map json-mode-map
   ("C-c b" . init-json--beautify-json-via-python))

  :custom
  (js-indent-level 4)

  :mode
  "\\.json\\(\\.j2\\)?\\'"

  :config
  (defun init-json--beautify-json-via-python ()
    "See, e.g., https://emacs.stackexchange.com/a/12152/599"
    (interactive)
    (save-excursion
      (shell-command-on-region (point-min) (point-max)
                               "python -m json.tool"
                               (buffer-name) t)))

  (defun init-json--beautify-json-via-js ()
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
  ;; TODO: Enable jq-mode with jq in JSON but yq in YAML.
  :ensure-system-package
  (jq . "sudo apt install -y jq")
  (yq . "pip install yq"))


(use-package devdocs
  :hook
  (json-mode . (lambda () (setq-local devdocs-current-docs '("jq")))))


(use-package lsp-mode
  :hook
  (json-mode . (lambda () (init-lsp-lsp-mode-hook 'json-ls))))

;;; 65-json.el ends here
