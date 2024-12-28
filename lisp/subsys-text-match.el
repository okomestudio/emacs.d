;;; subsys-text-match.el --- Text Match Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the text match subsystem.
;;
;;; Code:

(use-package rg
  :ensure-system-package (rg . "sudo apt install -y ripgrep")
  :config (rg-enable-default-bindings))

(use-package wgrep
  ;; Writable grep buffer and apply the changes to files.

  ;; With `rg':
  ;;
  ;;   - `e' starts edit mode
  ;;   - `C-x C-s' applies changes (but do not save yet)
  ;;   - `wgrep-save-all-buffers' saves changes to files
  ;;   - `C-c C-k' discards changes

  ;; With `embark':
  ;;
  ;; After embark export (`C-.' and `E' in consult minibuffer),
  ;;
  ;;   - `C-x C-q' and `C-c C-p' starts edit
  ;;   - `C-c C-e' applies changes
  ;;   - `C-c C-k' discards changes
  )

(provide 'subsys-text-match)
;;; subsys-text-match.el ends here
