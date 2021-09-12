;;; init-ansible.el --- Ansible  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ansible
  :hook
  (((text-mode fundamental-mode) . ts/ansible-mode-hook)
   (ansible-hook . 'ansible-auto-decrypt-encrypt))

  :config
  (defun ts/ansible-mode-hook ()
    (let ((root-path (ansible-find-root-path)))
      (when root-path
        (ansible 1)
        (setq ansible-vault-password-file (concat root-path "/.vault-password"))))))

(provide 'init-ansible)
;;; init-ansible.el ends here
