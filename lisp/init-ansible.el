;;; init-ansible.el --- Ansible  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ansible
  :disabled
  :mode ("\\.ya?ml\\'")
  :custom (ansible-dir-search-limit 20)
  :config
  (add-hook 'ansible-hook 'ts/ansible-mode-hook)

  (defun ts/ansible-mode-hook ()
    (let ((root-path (ansible-find-root-path)))
      (when root-path
        (setq ansible-vault-password-file (concat root-path "/.vault-password"))
        (ansible-auto-decrypt-encrypt) )
      ))
  )

(provide 'init-ansible)
;;; init-ansible.el ends here
