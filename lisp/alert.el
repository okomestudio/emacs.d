;;; alert.el --- Alert  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Alert configuration.
;;
;;; Code:

(use-package alert
  :demand t
  :custom ((alert-default-style 'notifications))
  :config
  ;; Use `notify-move-time' with `cancel-timer' to suppress this alert
  (setq notify-move-time
        (run-with-timer
         1800 1800
         (lambda ()
           (alert "Move away from computer!"
                  :title "MOVE!!"
                  :severity 'high)))))

;;; alert.el ends here
