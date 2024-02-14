;;; 80-gnus.el --- Gnus  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure Gnus.
;;
;;; Code:

(use-package gnus
  :straight nil
  :custom
  (gnus-home-directory (no-littering-expand-var-file-name "gnus/"))

  :config
  (defcustom ok-gnus-smtp-accounts nil
    "List of SMTP servers associated with sender email address.

Each list item is a list of form `(addr server, port usr)', where
`addr' is sender email address, `server' is SMTP server, `port'
is port, and `usr' is SMTP login."
    :type 'list
    :group 'ok)

  (setq gnus-summary-insert-old-articles t
        gnus-summary-line-format "%U%R%z%I%(%[%o: %-23,23f%]%) %s\\n"
        message-directory (concat gnus-home-directory "Mail/"))

  (defun ok-gnus--send-message ()
    "Set SMTP server from a list and send mail."
    (interactive)
    (message-remove-header "X-Message-SMTP-Method")
    (let ((sender (message-fetch-field "From")))
      (loop for (addr server port usr) in ok-gnus-smtp-accounts
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

  (add-hook 'gnus-message-setup-hook
            #'(lambda ()
                (local-set-key (kbd "C-c C-c") 'ok-gnus--send-message))))


(use-package gnus-group
  :straight nil
  :config
  (defhydra hydra-gnus-group (:color pink :hint nil)
    "
^Group^
^^^^^^^^^^^---------------------------------
_m_: create new message
_g_: get new messages
"
    ("m" gnus-group-new-mail)
    ("g" gnus-group-get-new-news)
    ("c" nil "cancel")
    ("q" gnus-group-exit "quit" :color blue))

  (define-key gnus-group-mode-map "." 'hydra-gnus-group/body))


(use-package gnus-sum
  :straight nil
  :config
  (defhydra hydra-gnus-summary (:color pink :hint nil)
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
    ("c" nil "cancel")
    ("q" gnus-summary-exit "quit" :color blue))

  (define-key gnus-summary-mode-map "." 'hydra-gnus-summary/body))


(use-package gnus-art
  :straight nil
  :config
  (defhydra hydra-gnus-article (:color pink :hint nil)
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
    ("c" nil "cancel"))

  (define-key gnus-article-mode-map "." 'hydra-gnus-article/body))

;; Local Variables:
;; nameless-aliases: (("" . "ok-gnus"))
;; End:
;;; 80-gnus.el ends here
