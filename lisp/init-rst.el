;;; init-rst.el --- Rst  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rst-mode
  :ensure nil
  :hook ((rst-mode . remove-trailing-whitespaces-on-save)))

(provide 'init-rst)
;;; init-rst.el ends here
