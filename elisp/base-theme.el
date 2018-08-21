;;; base-theme --- Custom themes
;;; Commentary:
;;; Code:

(use-package material-theme
  :defer t)

;; Zero dark theme
;; https://github.com/NicolasPetton/zerodark-theme
(use-package zerodark-theme
  :defer t)

(use-package spacemacs-theme
  :defer t)

(use-package monokai-theme
  :defer t)

(use-package kaolin-themes
  :config
  (load-theme 'kaolin-valley-dark t))

(provide 'base-theme)
;;; base-theme ends here
