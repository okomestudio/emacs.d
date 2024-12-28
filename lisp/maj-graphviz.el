;;; maj-graphviz.el --- Graphviz Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the Graphviz major mode.
;;
;;; Code:

(use-package graphviz-dot-mode
  :custom (graphviz-dot-indent-width 2)
  :ensure-system-package (dot . "sudo apt install -y graphviz"))

(provide 'maj-graphviz)
;;; maj-graphviz.el ends here
