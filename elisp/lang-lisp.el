;;; lang-lisp.el --- Lisp lang
;;; Commentary:
;;; Code:

(use-package slime
  :mode
  ("\\.lisp$" . slime-mode)
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl"
        slime-net-coding-system 'utf-8-unix
        slime1-contribs '(slime-fancy)))

(provide 'lang-lisp)
;;; lang-lisp ends here
