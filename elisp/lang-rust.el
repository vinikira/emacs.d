;;; lang-rust.el --- Rust lang
;;; Commentary:
;;; Code:

;; rust-mode
;; https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :ensure t
  :config  (setq rust-format-on-save t
                 company-tooltip-align-annotations t))

;; cargo-mode for all the cargo related operations
;; https://github.com/kwrooijen/cargo.el
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

;; racer mode
;; https://github.com/racer-rust/emacs-racer
(use-package racer
  :after (rust-mode)
  :hook ((rust-mode . racer-mode)
	 (racer-mode . flycheck-rust-setup)
	 (racer-mode . eldoc-mode))
  :config
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t))

(provide 'lang-rust)
;;; lang-rust ends here.
