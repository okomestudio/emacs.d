;;; init-graphviz.el --- Graphviz  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package graphviz-dot-mode
  :custom
  (graphviz-dot-indent-width 2)

  :ensure-system-package
  (dot . "sudo apt install -y graphviz"))


(provide 'init-graphviz)
;;; init-graphviz.el ends here
