;;; init-gnus.el --- Gnus  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package gnus
  :preface
  (defcustom init-gnus-smtp-accounts '()
    "List of SMTP servers associated with sender email address.

Each list item is:

  (<sender email address> <SMTP server> <port> <SMTP login>)."
    :type 'list
    :group 'ts)

  (defun init-gnus--send-message ()
    "Set SMTP server from list of multiple ones and send mail."
    (interactive)
    (message-remove-header "X-Message-SMTP-Method")
    (let ((sender (message-fetch-field "From")))
      (loop for (addr server port usr) in init-gnus-smtp-accounts
            when (string-match (format "\\(^\\|<\\)%s\\(>\\|$\\)" addr)
                               sender)
            do (message-add-header
                (format "X-Message-SMTP-Method: smtp %s %d %s"
                        server port usr)))
      (let ((xmess (message-fetch-field "X-Message-SMTP-Method")))
        (if (not xmess)
            (error (concat "SMTP server cannot be determined for %s."
                           " Use a known email address")
                   sender)
          (message (format "Sending message using '%s' with config '%s'"
                           sender xmess))
          (message-send-and-exit)))))

  :init
  ;; Some init timing issue prevents the use of :custom, so do them here.
  (setq gnus-home-directory "~/.local/var/gnus/")
  (ensure-directory-exists gnus-home-directory)

  (setq gnus-directory (concat gnus-home-directory "news/")
        gnus-dribble-directory gnus-home-directory
        gnus-init-file (concat gnus-home-directory "gnus.el")
        gnus-startup-file (concat gnus-home-directory ".newsrc")
        gnus-summary-insert-old-articles t
        gnus-summary-line-format "%U%R%z%I%(%[%o: %-23,23f%]%) %s\\n"
        message-directory (concat gnus-home-directory "mail/"))

  :config
  (add-hook 'gnus-message-setup-hook
            '(lambda ()
               (local-set-key (kbd "C-c C-c") 'init-gnus--send-message))))


(provide 'init-gnus)
;;; init-gnus.el ends here
