;;; init-ini.el --- Ini  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package any-ini-mode
  :ensure nil
  :mode ".*\\.ini$" ".*\\.conf$" ".*\\.service$"
  :init
  (ensure-file-from-url
   "https://www.emacswiki.org/emacs/download/any-ini-mode.el"))

(provide 'init-ini)
;;; init-ini.el ends here
