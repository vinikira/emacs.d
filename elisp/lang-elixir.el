;;; lang-elixir.el --- Elixir Lang
;;; Commentary:
;;; Code:
(use-package elixir-mode)

(use-package alchemist
  :config
  (add-hook 'elixir-mode-hook 'alchemist-mode))

(use-package flycheck-mix
  :commands (flycheck-mix-setup))

(provide 'lang-elixir)
;;; lang-elixir ends here
