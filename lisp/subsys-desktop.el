;;; subsys-desktop.el --- Desktop  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the desktop subsystem for session persistence.
;;
;;; Code:

(use-package desktop-serde)

(use-package desktop
  ;; Save the Emacs state across sessions.
  :custom ((desktop-auto-save-timeout 180)
           (desktop-modes-not-to-save '(eww-mode tags-table-mode)))
  :init
  (when-let* ((profile (or (getenv "EMACS_DESKTOP_PROFILE") "default")))
    (when-let* ((_ (not (string-empty-p profile)))
                (dir (directory-file-name
                      (fs-emacs-var "desktop"
                                    emacs-version ; or `comp-native-version-dir'?
                                    profile))))
      (make-directory dir t)
      (setq-default desktop-dirname dir)
      (setopt desktop-path (list desktop-dirname))

      ;; Loading the feature will set up after-init hook to actually
      ;; read a previously saved session. The session read will be
      ;; skipped when Emacs is launched with `--no-desktop` option.
      (desktop-save-mode 1)))

  :config
  (require 'desktop-serde)       ; ensures application of enhancements

  ;; Use if any globals should be saved.
  (add-to-list 'desktop-globals-to-save 'safe-local-variable-directories))

(provide 'subsys-desktop)
;;; subsys-desktop.el ends here
