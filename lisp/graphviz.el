;;; graphviz.el --- Graphviz  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure Graphviz related utilities.
;;
;;; Code:

(use-package graphviz-dot-mode
  :custom (graphviz-dot-indent-width 2)
  :ensure-system-package (dot . "sudo apt install -y graphviz"))

;;; graphviz.el ends here
