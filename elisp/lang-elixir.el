;;; lang-elixir.el --- Elixir Lang
;;; Commentary:
;;; Code:
(use-package alchemist
  :config
  (add-hook 'elixir-mode-hook 'alchemist-mode))

(use-package flycheck-mix
  :commands (flycheck-mix-setup))

(use-package elixir-mode)

(provide 'lang-elixir)
;;; lang-elixir ends here
