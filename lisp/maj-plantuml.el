;;; maj-plantuml.el --- PlantUML Major Mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Set up the PlantUML major mode.
;;
;;; Code:

(use-package plantuml-mode
  :mode ("\\.p\\(\\|lant\\)uml\\'" . plantuml-mode)
  :ensure-system-package (plantuml . "sudo apt install -y plantuml")
  :custom
  (plantuml-default-exec-mode 'jar)
  (plantuml-indent-level 4)
  (plantuml-jar-path "/usr/share/plantuml/plantuml.jar")

  :init
  (with-eval-after-load 'ob-plantuml
    (setopt org-plantuml-jar-path plantuml-jar-path)))

(use-package image-mode
  :straight (:type built-in)
  :bind ( :map image-mode-map
          ("C-c o" . image-preview-with-external-app) )
  :config
  (defun image-preview-with-external-app ()
    (interactive)
    (let* ((image-type (image-type-from-buffer))
           (preview-file (format "plantuml-mode-preview.%s" image-type))
           (preview-command (pcase image-type
                              ("svg" "display")
                              (_ "display")))
           (image (image--get-image)))
      (with-temp-buffer
        (let ((file (plist-get (cdr image) :file)))
          (if file
              (if (not (file-exists-p file))
                  (error "File %s no longer exists" file)
                (insert-file-contents-literally file))
            (insert (plist-get (cdr image) :data))))
        (let ((coding-system-for-write 'utf-8))
          (write-region (point-min) (point-max) preview-file)))
      (shell-command (format "%s %s &" preview-command preview-file)))))

(provide 'maj-plantuml)
;;; maj-plantuml.el ends here
