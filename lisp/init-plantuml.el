;;; init-plantuml.el --- Plantuml  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst ts/path-plantuml (expand-file-name "/usr/local/share/plantuml/plantuml.jar")
  "Path to PlantUML JAR.")

(use-package plantuml-mode
  :custom
  ((plantuml-default-exec-mode 'jar)
   (plantuml-jar-path ts/path-plantuml))

  :mode
  ("\\.plantuml\\'" . plantuml-mode)
  ("\\.puml\\'" . plantuml-mode))

(provide 'init-plantuml)
;;; init-plantuml.el ends here
