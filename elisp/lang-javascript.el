;;; lang-javascript.el --- Javascript lang
;;; Commentary:
;;; Code:

;; js2-mode
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :delight "EcmaScript"
  :hook ((js-mode . js2-minor-mode)
	 (js2-mode . company-mode)
	 (js2-mode . prettify-symbols-mode)
         (js2-mode . js2-imenu-extras-mode))
  :interpreter (("node" . js2-mode)
		("node" . js2-jsx-mode))
  :bind (:map js2-mode-map
              (("C-c f b" . vs/format-standardjs-buffer)
               ("C-c f w s" . lsp-ui-peek-find-workspace-symbol)))
  :mode ("\\.js$" . js2-mode)
  :config
  (custom-set-variables '(js2-mode-show-parse-errors nil)
                        '(js2-mode-show-strict-warnings nil)
                        '(js2-bounce-indent-p t))
  (setq js2-include-node-externs t
	js2-highlight-level 3
	js2-strict-missing-semi-warning nil
	flycheck-check-syntax-automatically '(mode-enabled save))
  (setq-default indent-tabs-mode nil
                js-indent-level 2
        	js2-basic-offset 2
		flycheck-temp-prefix ".flycheck"
		flycheck-disabled-checkers '(javascript-jshint)
		flycheck-checkers '(javascript-standard javascript-eslint)))

;; js2-refactor :- refactoring options for emacs
;; https://github.com/magnars/js2-refactor.el
(use-package js2-refactor
  :delight
  :after (js2-mode)
  :hook ((js2-mode . js2-refactor-mode))
  :config
  (js2r-add-keybindings-with-prefix "C-c j r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill))

(use-package xref-js2
  :delight
  :if (executable-find "ag")
  :after (js2-mode)
  :config
  (define-key js2-mode-map (kbd "M-.") nil)
  :hook ((js2-mode .
            (lambda ()
              (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))))

(use-package json-mode
  :mode
  ("\\.json$" . json-mode))

(use-package rjsx-mode
  :mode
  ("\\.jsx$" . rjsx-mode))

(use-package vue-mode
  :mode
  ("\\.vue$" . vue-mode))

;; indium: javascript awesome development environment
;; https://github.com/NicolasPetton/indium
(use-package indium
  :after js2-mode
  :hook ((js2-mode . indium-interaction-mode))
  :bind (:map indium-interaction-mode-map
              ("C-x C-e" . indium-eval-last-node)
              ("C-c i q" . (lambda ()
                             (interactive)
                             (indium-quit)
                             (revert-buffer t t)))
              ("C-c i c" . indium-connect))
  :config (delight indium-interaction-mode))

;; typescript mode
;; https://github.com/ananthakumaran/tide
(defun setup-tide-mode ()
  "Setup tide mode."
  (interactive)
  (tide-setup)
  (setq-default company-tooltip-align-annotations t)
  (tide-hl-identifier-mode +1))

(use-package tide
  :ensure t
  :mode ("\\.ts\\'" . typescript-mode)
  :after (typescript-mode company flycheck)
  :bind (:map tide-mode-map
              ("C-c C-d" . tide-jsdoc-template)
              ("C-c t f" . tide-organize-imports))
  :hook ((typescript-mode . setup-tide-mode)
         (typescript-mode . tide-hl-identifier-mode)))

(provide 'lang-javascript)
;;; lang-javascript ends here
