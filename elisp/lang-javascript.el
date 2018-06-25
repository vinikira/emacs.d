;;; lang-javascript.el --- Javascript lang
;;; Commentary:
;;; Code:

;; js2-mode
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :delight "EcmaScript"
  :bind (:map js2-mode-map
              (("C-x C-e" . js-send-last-sexp)
               ("C-M-x" . js-send-last-sexp-and-go)
               ("C-c C-b" . js-send-buffer-and-go)
               ("C-c C-l" . js-load-file-and-go)))
  :mode
  ("\\.js$" . js2-mode)
  :config
  (custom-set-variables '(js2-strict-inconsistent-return-warning nil))
  (custom-set-variables '(js2-strict-missing-semi-warning nil))
  (setq js2-global-externs '("define" "require" "app"))
  (setq js2-include-node-externs t)
  (setq js2-pretty-multiline-declarations nil)
  (setq-default indent-tabs-mode nil))

(use-package json-mode
  :mode
  ("\\.json$" . json-mode))

(setq js-indent-level 2)
(setq js2-indent-level 2)
(setq js2-basic-offset 2)

;; tern :- IDE like features for javascript and completion
;; http://ternjs.net/doc/manual.html#emacs
(use-package tern
  :defer t
  :delight
  :config
  (defun my-js-mode-hook ()
    "Hook for `js-mode'."
    (set (make-local-variable 'company-backends)
	 '((company-tern company-files company-yasnippet))))
  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil)
  (add-hook 'js2-mode-hook 'my-js-mode-hook)
  (add-hook 'js2-mode-hook 'company-mode)
  (add-hook 'js2-mode-hook 'tern-mode))

;; company backend for tern
;; http://ternjs.net/doc/manual.html#emacs
(use-package company-tern
  :after tern
  :if (executable-find "tern")
  :config
  (setq company-tooltip-align-annotations t))

;; Run a JavaScript interpreter in an inferior process window
;; https://github.com/redguardtoo/js-comint
(use-package js-comint
  :config
  (setq inferior-js-program-command "node"))

;; js2-refactor :- refactoring options for emacs
;; https://github.com/magnars/js2-refactor.el
(use-package js2-refactor
  :diminish
  :after (js2-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c j r")
  (add-hook 'js2-mode-hook 'js2-refactor-mode))

;;prettier-js - format javascript source codes
;;https://github.com/prettier/prettier-emacs
;; (use-package prettier-js
;;   :config
;;   (add-hook 'js2-mode-hook 'prettier-js-mode)
;;   ;;eslint support
;;   (add-hook 'js2-mode-hook (lambda () (add-hook 'after-save-hook #'eslint-fix-file t))))


(use-package rjsx-mode
  :ensure t
  :mode
  ("\\.jsx$" . js2-jsx-mode))

(use-package vue-mode
  :mode
  ("\\.vue$" . vue-mode))

;; indium: javascript awesome development environment
;; https://github.com/NicolasPetton/indium
(use-package indium
  :after js2-mode
  :bind (:map js2-mode-map
              ("C-c C-l" . indium-eval-buffer))
  :hook ((js2-mode . indium-interaction-mode))
  :config (diminish 'indium-interaction-mode))

;; typescript mode
;; https://github.com/ananthakumaran/tide
(defun setup-tide-mode ()
  "Setup typscript mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(use-package tide
  :mode (("\\.ts\\'" . typescript-mode))
  :config
  (setup-tide-mode)
  (setq company-tooltip-align-annotations t)
  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(provide 'lang-javascript)
;;; lang-javascript ends here
