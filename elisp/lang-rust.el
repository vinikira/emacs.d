;;; lang-rust.el --- Rust lang
;;; Commentary:
;;; Code:

;; rust-mode
;; https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :init  (setq rust-format-on-save t
                 company-tooltip-align-annotations t))

(use-package flycheck-rust
  :after rust-mode
  :hook ((rust-mode . flycheck-rust-setup)))

;; cargo-mode for all the cargo related operations
;; https://github.com/kwrooijen/cargo.el
(use-package cargo
  :hook ((rust-mode . cargo-minor-mode)))

;; racer mode
;; https://github.com/racer-rust/emacs-racer
(use-package racer
  :hook ((rust-mode . racer-mode)
	 (racer-mode . eldoc-mode))
  :config
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common))

(provide 'lang-rust)
;;; lang-rust.el ends here
