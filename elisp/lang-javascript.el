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
  (custom-set-variables '(js2-mode-show-parse-errors nil))
  (custom-set-variables '(js2-mode-show-strict-warnings nil))
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (setq js2-global-externs '("define" "require" "app")
	js2-include-node-externs t)
  (setq-default indent-tabs-mode nil
                js-indent-level 2
		js2-basic-offset 2)
  (cond ((executable-find "standard") '(flycheck-checker . javascript-standard))
        ((executable-find "eslint_d") '(flycheck-checker . javascript-eslint))))

;; js2-refactor :- refactoring options for emacs
;; https://github.com/magnars/js2-refactor.el
(use-package js2-refactor
  :delight
  :after (js2-mode)
  :hook (js2-mode . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c j r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill))

(use-package xref-js2
  :delight
  :if (executable-find "ag")
  :after (js2-mode)
  :config
  (add-hook 'js2-mode-hook
            (lambda ()
              (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package json-mode
  :mode
  ("\\.json$" . json-mode))

;; tern :- IDE like features for javascript and completion
;; http://ternjs.net/doc/manual.html#emacs
(use-package tern
  :defer t
  :delight
  :if (executable-find "tern")
  :hook (js2-mode . tern-mode)
  :config
  (add-to-list 'tern-command "--no-port-file" 'append)
  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil))

;; company backend for tern
;; http://ternjs.net/doc/manual.html#emacs
(use-package company-tern
  :defer t
  :after tern
  :if (executable-find "tern")
  :init (add-to-list 'company-backends 'company-tern)
  :config
  (setq company-tooltip-align-annotations t))

(use-package rjsx-mode
  :mode
  ("\\.jsx$" . js2-jsx-mode))

(use-package vue-mode
  :mode
  ("\\.vue$" . vue-mode))

;; indium: javascript awesome development environment
;; https://github.com/NicolasPetton/indium
(use-package indium
  :defer t
  :after js2-mode
  :bind (:map js2-mode-map
              ("C-c C-l" . indium-eval-buffer))
  :config (delight indium-interaction-mode))

;; typescript mode
;; https://github.com/ananthakumaran/tide
(use-package tide
  :mode (("\\.ts\\'" . typescript-mode))
  :config
  (setq company-tooltip-align-annotations t)
  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook
	    '(lambda ()
	       "Setup typscript mode."
	       (interactive)
	       (setq flycheck-check-syntax-automatically '(save mode-enabled))
	       (tide-hl-identifier-mode +1))))

(provide 'lang-javascript)
;;; lang-javascript ends here
