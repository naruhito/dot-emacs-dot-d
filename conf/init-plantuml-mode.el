;;; plantuml-mode.el --- Major mode for PlantUML    -*- lexical-binding: t; -*-

;; M-x plantuml-download-jar

;; java -jar ~/.emacs.d/var/plantuml/plantuml.jar sample.pu

(setq plantuml-jar-path "~/.emacs.d/var/plantuml/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)
(setq plantuml-options "-charset UTF-8")
