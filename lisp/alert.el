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
  (defcustom do-this-now-interval 2400
    "Time in second before alert trigger.")

  (defvar do-this-now--timer nil
    "Timer object for alert. Use `cancel-timer' to suppress the alert.")

  (defun do-this-now-alert ()
    (alert "Move away from computer!"
           :title "MOVE!!"
           :severity 'high)
    (do-this-now-setup-next-alert))

  (defun do-this-now-start-timer-for-next-alert ()
    (when do-this-now--timer
      (cancel-timer do-this-now--timer))
    (setq do-this-now--timer
          (run-with-timer do-this-now-interval nil #'do-this-now-alert)))

  (defun do-this-now--hook ()
    (message "BEG do-this-now-timer-for-next-alert")
    (do-this-now-start-timer-for-next-alert)
    (remove-hook 'post-command-hook #'do-this-now--hook)
    (message "END do-this-now-timer-for-next-alert"))

  (defun do-this-now-setup-next-alert ()
    (message "BEG do-this-now-setup-next-alert")
    (add-hook 'post-command-hook #'do-this-now--hook)
    (message "END do-this-now-setup-next-alert"))

  (do-this-now-setup-next-alert))

;;; alert.el ends here
