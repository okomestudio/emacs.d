;;; gnus.el --- Gnus  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Gnus configuration for Email.
;;
;;; Code:

(use-package message
  :straight nil
  :bind (;
         :map message-mode-map
         ("C-c C-c" . message-send-and-exit-via-sender-email))
  :hook (message-setup . message--deactivate-hydra)
  :custom ((message-auto-save-directory (no-littering-expand-var-file-name "message/"))
           (message-directory (no-littering-expand-var-file-name "message/Mail/")))
  :config
  (defcustom message-smtp-accounts nil
    "`alist' mapping from sender email address to SMTP server.
Each item in the list is of form `(addr . (:server server :port
port :user usr)', where `addr' is sender email address, `server'
is SMTP server, `port' is port, and `usr' is SMTP login username."
    :type 'alist
    :group 'ok)

  (defun message-send-and-exit-via-sender-email ()
    "Send message via SMTP server based on sender email address."
    (interactive)
    (message-remove-header "X-Message-SMTP-Method")
    (let* ((from (message-fetch-field "From"))
           (sender-email (let* ((re-email "[[:alnum:].]+@[[:alnum:].]+")
                                (re (format "\\(?:^\\(%s\\)$\\|<\\(%s\\)>\\)"
                                            re-email re-email)))
                           (string-match re from)
                           (or (match-string 1 from)
                               (match-string 2 from))))
           (smtp-server (cdr (assoc sender-email message-smtp-accounts))))
      (unless smtp-server
        (error "SMTP server not found for '%s'" from))

      (let ((x-message-smtp-method
             (format "X-Message-SMTP-Method: smtp %s %d %s"
                     (plist-get smtp-server :server)
                     (plist-get smtp-server :port)
                     (plist-get smtp-server :user))))
        (when (yes-or-no-p (format "Send message via %s?"
                                   x-message-smtp-method))
          (message-add-header x-message-smtp-method)
          (message "Sending message for '%s' via '%s'"
                   from x-message-smtp-method)
          (message-send-and-exit)))))

  (defun message--deactivate-hydra ()
    "Deactivate Hydra."
    ;; When invoked through Gnus, hydra may contaminate the keymap for
    ;; some reason. This is a workaround.
    (setq-local hydra-deactivate t)))

(use-package nnfolder
  :straight nil
  :config
  ;; NOTE: `nnfolder-directory' easily reverts to "~/Mail" if
  ;; `etc/gnus/newsrc.eld' exists. Delete that file and initialize
  ;; Gnus config once that happens.
  (setq nnfolder-directory (no-littering-expand-var-file-name
                            "message/Mail/archive")))

(use-package gnus
  :straight nil
  :custom ((gnus-directory (no-littering-expand-var-file-name "gnus/News/"))
           (gnus-default-directory (no-littering-expand-var-file-name "gnus/"))
           (gnus-home-directory (no-littering-expand-var-file-name "gnus/"))
           (gnus-summary-line-format "%U%R%z%I%(%[%o: %-23,23f%]%) %s\\n"))
  :config
  (setq gnus-summary-insert-old-articles t))

(use-package gnus-group
  :straight nil
  :bind (:map gnus-group-mode-map
              ("." . hydra-gnus-group/body))
  :config
  (defhydra hydra-gnus-group (gnus-group-mode-map "." :color pink :hint nil)
    "
^Group^
^^^^^^^^^^^---------------------------------
_m_: create new message
_g_: get new messages
"
    ("m" message-mail)
    ("g" gnus-group-get-new-news)
    ("." nil "cancel")
    ("q" gnus-group-exit "quit" :color blue)))

(use-package gnus-sum
  :straight nil
  :bind (:map gnus-summary-mode-map
              ("." . hydra-gnus-summary/body))
  :config
  (defhydra hydra-gnus-summary (gnus-summary-mode-map "." :color pink :hint nil)
    "
^Message Summary^ ^^             ^Threads
^^^^^^^^^^^---------------------------------
_o_: open         _f_: forward   _t_: toggle
_m_: create new   _A_: archive   ^ ^
_r_: reply all    _C_: copy
_R_: reply        _M_: move
"
    ("o" gnus-summary-show-article)
    ("m" gnus-summary-mail-other-window)
    ("r" gnus-summary-wide-reply-with-original)
    ("R" gnus-summary-reply-with-original)
    ("f" gnus-summary-mail-forward)
    ("A" gnus-summary-delete-article)
    ("C" gnus-summary-copy-article)
    ("M" gnus-summary-move-article)
    ("t" gnus-summary-toggle-threads)
    ("." nil "cancel")
    ("q" gnus-summary-exit "quit" :color blue)))

(use-package gnus-art
  :straight nil
  :bind (:map gnus-article-mode-map
              ("." . hydra-gnus-article/body))
  :config
  (defhydra hydra-gnus-article (gnus-article-mode-map "." :color pink :hint nil)
    "
^Message^
^^^^^^^^^^^---------------------------------
_r_: reply all
_R_: reply
_f_: forward
"
    ("r" gnus-article-wide-reply-with-original)
    ("R" gnus-article-reply-with-original)
    ("f" gnus-summary-mail-forward)
    ("." nil "cancel")))

(load (no-littering-expand-etc-file-name "gnus/init"))

;;; gnus.el ends here
