;;; init-plantuml.el --- Plantuml  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defconst init-plantuml-path-plantuml
  (expand-file-name "/usr/share/plantuml/plantuml.jar")
  "Path to PlantUML JAR.")


(use-package plantuml-mode
  :custom
  (org-plantuml-jar-path init-plantuml-path-plantuml)
  (plantuml-default-exec-mode 'jar)
  (plantuml-indent-level 4)
  (plantuml-jar-path init-plantuml-path-plantuml)

  :ensure-system-package
  (plantuml . "sudo apt install -y plantuml")

  :mode
  ("\\.plantuml\\'" . plantuml-mode)
  ("\\.puml\\'" . plantuml-mode))


(provide 'init-plantuml)
;;; init-plantuml.el ends here
