;;; base-theme --- Custom themes
;;; Commentary:
;;; Code:

(use-package material-theme
  :ensure t
  :defer t
  ;; :init
  ;; (load-theme 'material t)
  )

;; Zero dark theme
;; https://github.com/NicolasPetton/zerodark-theme
(use-package zerodark-theme
  :defer t
  :init
  (load-theme 'zerodark t)
  (zerodark-setup-modeline-format))

(use-package spacemacs-theme
  :defer t
  ;; :init
  ;; (load-theme 'spacemacs-dark t)
  )

(use-package monokai-theme
  :defer t
  ;; :init
  ;; (load-theme 'monokai t)
  )

(provide 'base-theme)
;;; base-theme ends here
