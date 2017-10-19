;; (use-package spacemacs-theme
;;   :defer t
;;   :init
;;   (load-theme 'spacemacs-dark t))

;; (use-package monokai-theme
;;   :defer t
;;   :init
;;   (load-theme 'monokai t))

(use-package zerodark-theme
  :defer t
  :init
  (load-theme 'zerodark t)
  (zerodark-setup-modeline-format))

(provide 'base-theme)
