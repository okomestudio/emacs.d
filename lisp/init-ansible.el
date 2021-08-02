;;; init-ansible.el --- Ansible  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ansible
  :after (yaml-mode)

  :custom
  (ansible-vault-password-file nil)

  :hook (((yaml-mode) . my-ansible-mode-hook)
         ((ansible) . ansible-auto-decrypt-encrypt))

  :config
  (defun my-ansible-mode-hook ()
    (if (locate-dominating-file default-directory "ansible.cfg")
        (progn (ansible 1)))))

(provide 'init-ansible)
;;; init-ansible.el ends here
