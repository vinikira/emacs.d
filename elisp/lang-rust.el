;;; lang-rust.el --- Rust lang
;;; Commentary:
;;; Code:

;; rust-mode
;; https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :ensure t
  :hook ((rust-mode . lsp-mode)
	 (rust-mode . lsp-rust-enable))
  :config  (setq rust-format-on-save t
                 company-tooltip-align-annotations t))

;; cargo-mode for all the cargo related operations
;; https://github.com/kwrooijen/cargo.el
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

;; rust language server protocol for emacs
;; https://github.com/emacs-lsp/lsp-rust
(use-package lsp-rust
  :after (lsp-mode)
  :config (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
  (push 'company-lsp company-backends))

(provide 'lang-rust)
;;; lang-rust ends here
