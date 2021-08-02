;;; init-json.el --- Json  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package jq-mode
  :ensure-system-package (jq . "sudo apt install jq"))

(use-package json-mode
  :bind
  (:map json-mode-map
   ("C-M-b" . ts/json-format))

  :config
  ;; See, e.g., https://emacs.stackexchange.com/a/12152/599
  (defun ts/json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min)
                             (point-max)
                             "python -m json.tool"
                             (buffer-name) t)))

  :mode "\\.json\\'" "\\.json.j2\\'"

  :init
  (setq js-indent-level 4))

(provide 'init-json)
;;; init-json.el ends here
