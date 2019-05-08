;;; lang-elixir.el --- Elixir Lang
;;; Commentary:
;;; Code:

(defun format-elixir-buffer ()
  "Format elixir buffer."
  (add-hook 'before-save-hook 'elixir-format nil t))

(use-package elixir-mode
  :hook ((elixir-mode . format-elixir-buffer)
	 (elixir-mode . flycheck-mix-setup))
  :mode (("\\.ex$" . elixir-mode)
	 ("\\.exs$" . elixir-mode)))

(use-package alchemist
  :hook (elixir-mode . alchemist-mode))

(use-package flycheck-mix)

(provide 'lang-elixir)
;;; lang-elixir ends here
