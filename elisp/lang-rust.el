;;; lang-rust.el --- Rust lang
;;; Commentary:
;;; Code:

;; rust-mode
;; https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :ensure t
  :config  (setq rust-format-on-save t))

;; add flycheck support for rust
;; https://github.com/flycheck/flycheck-rust
(use-package flycheck-rust
  :init (with-eval-after-load 'rust-mode
          (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

;; cargo-mode for all the cargo related operations
;; https://github.com/kwrooijen/cargo.el
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

;; racer-mode for getting IDE like features for rust-mode
;; https://github.com/racer-rust/emacs-racer
(use-package racer
  :hook (rust-mode . racer-mode)
  :bind (:map rust-mode-map
              (("C-c C-t" . racer-describe)))
  :config
  (progn
    ;; set racer rust source path environment variable
    (defun rust-src-path ()
      "Find rustup src path on system."
      (replace-regexp-in-string
       "\n$"
       ""
       (shell-command-to-string
        "echo `rustc --print sysroot`/lib/rustlib/src/rust/src")))
    (defun my-racer-mode-hook ()
      (set (make-local-variable 'company-backends)
           '((company-capf company-files))))
    (setq racer-rust-src-path (rust-src-path))
    (add-hook 'racer-mode-hook 'eldoc-mode)))

(provide 'lang-rust)
;;; lang-rust ends here
