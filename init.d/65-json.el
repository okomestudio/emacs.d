;;; 65-json.el --- json  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure json-ts-mode and related utilities.
;;
;;; Code:

(use-package json-ts-mode
  :mode "\\.json\\(\\.j2\\)?\\'"
  :bind (nil
         :map json-ts-mode-map
         ("C-c b" . json-ts-mode-format-code))
  :hook (json-ts-mode . lsp-deferred)
  :custom (json-ts-mode-indent-offset 2)
  :config
  (defun json-ts-mode-format-code ()
    (interactive)
    (prettier-js)))


(use-package jq-mode
  :after json-ts-mode
  :bind (nil
         :map json-ts-mode-map
         ("C-c C-j" . jq-interactively))
  :ensure-system-package (jq . "sudo apt install -y jq")
  :config
  (defun jq-interactively-on-json (&rest r)
    (when (derived-mode-p 'json-ts-mode)
      (setq-local jq-interactive-command "jq"
                  jq-interactive-font-lock-mode #'json-ts-mode
                  jq-interactive-default-options "")))
  (advice-add #'jq-interactively :before #'jq-interactively-on-json))

;; Local Variables:
;; nameless-aliases: (("" . "ok-json"))
;; End:
;;; 65-json.el ends here
