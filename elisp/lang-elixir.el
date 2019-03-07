;;; lang-elixir.el --- Elixir Lang
;;; Commentary:
;;; Code:
(use-package elixir-mode
  :mode (("\\.ex$" . elixir-mode)
	 ("\\.exs$" . elixir-mode)))

(use-package alchemist
  :hook (elixir-mode . alchemist-mode))

(use-package flycheck-mix
  :commands (flycheck-mix-setup))

(provide 'lang-elixir)
;;; lang-elixir ends here
