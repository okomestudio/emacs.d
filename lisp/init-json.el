;;; init-json.el --- JSON  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package json-mode
  :after (web-beautify)

  :bind
  (:map json-mode-map ("C-c b" . ts/beautify-json))

  :custom
  (js-indent-level 4)

  :mode
  ("\\.json\\'"
   "\\.json.j2\\'")

  :config
  (defun ts/beautify-json-via-python ()
    "See, e.g., https://emacs.stackexchange.com/a/12152/599"
    (interactive)
    (save-excursion
      (shell-command-on-region (point-min)
                               (point-max)
                               "python -m json.tool"
                               (buffer-name) t)))

  (defun ts/beautify-json ()
    (interactive)
    (save-excursion
      (let ((web-beautify-args '("-f" "-"
                                 "--end-with-newline"
                                 "--no-preserve-newlines"
                                 "--jslint-happy"
                                 "--wrap-line-length" "88")))
           (web-beautify-js)))))

(use-package jq-mode
  :ensure-system-package
  (jq . "sudo apt install jq"))

(provide 'init-json)
;;; init-json.el ends here
