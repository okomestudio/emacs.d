;;; 65-json.el --- JSON  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure JSON related utilities.
;;
;;; Code:

(use-package json-ts-mode
  :mode "\\.json\\(\\.j2\\)?\\'"
  :bind
  (;; no globals
   :map json-ts-mode-map
   ("C-c b" . ok-json--beautify-json-via-python))

  :custom
  (js-indent-level 4)

  :hook
  (json-ts-mode . (lambda ()
                    (setq-local devdocs-current-docs '("jq"))))
  (json-ts-mode . (lambda ()
                    (lsp-ensure-server 'json-ls)
                    (lsp-deferred)))

  :config
  (defun ok-json--beautify-json-via-python ()
    "See, e.g., https://emacs.stackexchange.com/a/12152/599"
    (interactive)
    (save-excursion
      (shell-command-on-region (point-min) (point-max)
                               "python -m json.tool"
                               (buffer-name) t)))

  (defun ok-json--beautify-json-via-web-beautify ()
    "Need to install `web-beautify'."
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

;; Local Variables:
;; nameless-aliases: (("" . "ok-json"))
;; End:
;;; 65-json.el ends here
