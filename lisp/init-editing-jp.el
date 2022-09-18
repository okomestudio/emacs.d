;;; init-editing-jp.el --- Editing Japanese  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :ensure nil
  :ensure-system-package
  (textlint . "sudo ~/.config/emacs/bin/prepare-textlint"))

(provide 'init-editing-jp)
;;; init-editing-jp.el ends here
