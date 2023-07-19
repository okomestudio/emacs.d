;;; init-secrets.el --- secrets.el  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; For this example of using this with KeepassXC, see
;; https://ph-uhl.com/emacs-password-management-on-linux/.
;;
;;; Code:


(use-package secrets
  :commands
  (secrets-search-items
   secrets-get-secret
   secrets-get-attributes)

  :config
  ;; Adds a patch to fix behavior with KeepassXC
  (defun secrets-unlock-item (collection item)
    "Unlock item labeled ITEM from collection labeled COLLECTION.
    If successful, return the object path of the item."
    (let ((item-path (secrets-item-path collection item)))
      (unless (secrets-empty-path item-path)
        (secrets-prompt
         (cadr
          (dbus-call-method
           :session secrets-service secrets-path secrets-interface-service
           "Unlock" `(:array :object-path ,item-path)))))
      item-path))

  ;; Adds a patch to fix behavior with KeepassXC
  (defun secrets-get-secret (collection item)
    "Return the secret of item labeled ITEM in COLLECTION.
    If there are several items labeled ITEM, it is undefined which
    one is returned.  If there is no such item, return nil.

    ITEM can also be an object path, which is used if contained in COLLECTION."
    (let ((item-path (secrets-unlock-item collection item)))
      (unless (secrets-empty-path item-path)
        (dbus-byte-array-to-string
         (nth 2
              (dbus-call-method
               :session secrets-service item-path secrets-interface-item
               "GetSecret" :object-path secrets-session-path)))))))


(provide 'init-secrets)
;;; init-secrets.el ends here
