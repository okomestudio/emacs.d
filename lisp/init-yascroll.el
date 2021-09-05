;;; init-yascroll.el --- Yascroll  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; yascroll.el - Yet Another Scroll Bar Mode for GNU Emacs
;; -------------------------------------------------------
;; https://github.com/emacsorphanage/yascroll
(use-package yascroll
  :init
  (ensure-file-from-github "emacsorphanage/yascroll/master/yascroll.el")

  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (when (not window-system)
                      (yascroll-bar-mode +1)))))
    (when (not window-system)
      (yascroll-bar-mode +1))))

(provide 'init-yascroll)
;;; init-yascroll.el ends here
