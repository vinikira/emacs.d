;;; lang-java.el --- Java Lang
;;; Commentary:
;;; Code:
(use-package cc-mode)

(use-package java-mode
  :ensure nil
  :mode ("\\.java$")
  :config
    (c-set-style "cc-mode")
    (setq tab-width 4
	indent-tabs-mode t
	c-basic-offset 4))

(provide 'lang-java)
;;; lang-java.el ends here
