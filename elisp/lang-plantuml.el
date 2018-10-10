;;; lang-plantuml.el --- Plantuml
;;; Commentary:
;;; Code:

;; plantuml-mode
;; https://github.com/skuro/plantuml-mode
(use-package plantuml-mode
  :mode ("\\.plantuml\\'" . plantuml-mode)
  :config
  (let ((plantuml-directory "~/.emacs.d/private/")
      (plantuml-link "https://superb-dca2.dl.sourceforge.net/project/plantuml/plantuml.jar"))
  (let ((plantuml-target (concat plantuml-directory "plantuml.jar")))
    (if (not (file-exists-p plantuml-target))
        (progn (message "Downloading plantuml.jar")
               (shell-command
                (mapconcat 'identity (list "wget" plantuml-link "-O" plantuml-target) " "))
               (kill-buffer "*Shell Command Output*")))
    (setq org-plantuml-jar-path plantuml-target
	  plantuml-jar-path plantuml-target
	  plantuml-output-type "svg"))))

;; flycheck-plantuml
;; https://github.com/alexmurray/flycheck-plantuml
(use-package flycheck-plantuml
  :config (flycheck-plantuml-setup))

(provide 'lang-plantuml)
;;; lang-plantuml ends here
