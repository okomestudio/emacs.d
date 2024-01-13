;;; init-prog.el --- Programming modes  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package prog-mode
  :straight nil
  :hook
  (prog-mode . (lambda ()
                 (add-hook 'local-write-file-hooks
                           #'(lambda () (save-excursion
                                          (delete-trailing-whitespace))))
                 (show-paren-mode))))


;; modes derived from prog-mode:
(require 'init-c)
(require 'init-docker)
(require 'init-elisp)
(require 'init-graphviz)
(require 'init-json)
(require 'init-kotlin)
(require 'init-plantuml)
(require 'init-python)
(require 'init-rust)
(require 'init-scala)
(require 'init-shell)
(require 'init-sql)
;; (require 'init-tern)
(require 'init-webmode)


;; modes not derived from prog-mode
(require 'init-ansible)
(require 'init-git)
(require 'init-restclient)  ;; derive fundamental-mode


(provide 'init-prog)
;;; init-prog.el ends here
