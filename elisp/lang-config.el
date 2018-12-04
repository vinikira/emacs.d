;;; lang-config.el --- Lang config file
;;; Commentary: Lang for config type files
;;; Code:

(use-package yaml-mode
  :mode ("\\.yaml|.yml$" . yaml-mode))

(use-package dockerfile-mode
  :mode ("\\Dockerfile$" . dockerfile-mode))

(use-package docker-compose-mode)

(use-package toml-mode
  :mode ("\\.toml$" . toml-mode))

(provide 'base-extensions)
;;; base-extensions ends here
