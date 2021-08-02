;;; init-prog.el --- Prog  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . remove-trailing-whitespaces-on-save)
         (prog-mode . show-paren-mode)))

(provide 'init-prog)
;;; init-prog.el ends here
