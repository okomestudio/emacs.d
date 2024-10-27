;;; completion-inactive.el --- completion-inactive  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Inactive modules for completion.
;;
;;; Code:

(use-package tabnine
  ;; One-time `tabnine-install-binary' may be necessary.
  ;;
  ;; TODO: Resolve `tabnine-util--infer-indentation-offset' error
  :commands (tabnine-start-process)
  :bind (:map tabnine-completion-map
	            ("TAB" . nil)
              ("<tab>" . nil))
  :hook ((prog-mode . tabnine-mode)
         (kill-emacs . tabnine-kill-process))
  :config (tabnine-start-process))

(use-package corfu-info    ; or use popupinfo
  ;; `M-h' toggles the info on selected item.
  :straight nil)

;;; completion-inactive.el ends here
