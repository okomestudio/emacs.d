;;; subsys-spelling.el --- Spelling Subsystem  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configure the spelling subsystem.
;;
;;; Code:

(use-package jinx
  ;; A fast JIT spell checker using Enchant.
  ;;
  ;; NOTE(2025-12-22): Use this in place of ispell/aspell (2025-12-22).
  :bind (("C-;" . jinx-correct))
  :custom (jinx-languages "en_US")
  :hook (org-mode . jinx-mode)
  :ensure-system-package
  (enchant-2 . "sudo apt install -y libenchant-2-dev pkgconf")
  :config
  ;; \\cc is Chinese category character, which includes Han + Hiragana +
  ;; Katakana + other CJK scripts.
  (add-to-list 'jinx-exclude-regexps '(t "\\cc"))

  (let ((ul (face-attribute 'flycheck-error :underline)))
    (set-face-attribute 'jinx-misspelled nil
                        :underline (append ul '(:style wave)))))

(provide 'subsys-spelling)
;;; subsys-spelling.el ends here
