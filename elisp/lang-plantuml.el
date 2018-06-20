;;; lang-plantuml.el --- Plantuml
;;; Commentary:
;;; Code:

;; plantuml-mode
;; https://github.com/skuro/plantuml-mode
(use-package plantuml-mode
  :mode ("\\.plantuml\\'" . plantuml-mode)
  :config
  (setq plantuml-jar-path "/opt/plantuml/plantuml.jar"
	plantuml-output-type "svg"))

;; flycheck-plantuml
;; https://github.com/alexmurray/flycheck-plantuml
(use-package flycheck-plantuml
  :config (flycheck-plantuml-setup))

;;; lang-plantuml ends here
