;;; lang-elm.el --- Elm lang
;;; Commentary:
;;; Code:
(use-package elm-mode
  :mode ("\\.elm$")
  :config (add-to-list 'company-backends 'company-elm))

(provide 'lang-elm)
;;; lang-elm ends here
