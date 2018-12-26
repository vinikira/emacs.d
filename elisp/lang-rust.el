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

(use-package racer
  :after (rust-mode)
  :hook ((rust-mode . racer-mode)
	 (racer-mode . flycheck-rust-setup))
  :config
  (eldoc-mode 1)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t))

;; rust language server protocol for emacs
;; https://github.com/emacs-lsp/lsp-rust
;; (use-package lsp-rust
;;   :hook ((rust-mode . lsp-mode)
;; 	 (rust-mode . lsp-rust-enable))
;;   :config
;;   (setq lsp-rust-rls-command '("rustup" "run" "stable" "rls"))
;;   (push 'company-lsp company-backends))

(provide 'lang-rust)
;;; lang-rust ends here
