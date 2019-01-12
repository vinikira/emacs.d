;;; package --- Python Lang
;;; Commentary:
;;; Contains my python configs
;;; packages to install before use: pip install flake8 jedi autopep8 rope yapf ipython
;;; archlinux: yay -S autopep8 flake8 python-jedi python-rope yapf ipython
;;; Code:

(use-package python
  :mode ("\\.py" . python-mode)
  :config (setq python-shell-interpreter "ipython"
		python-shell-interpreter-args "-i --simple-prompt"))

(use-package elpy
  :hook ((python-mode . elpy-mode)
	 (python-mode . elpy-enable))
  :custom
  (elpy-rpc-backend "jedi")
  :bind (:map elpy-mode-map
	      ("M-." . elpy-goto-definition)
	      ("M-," . pop-tag-mark)
	      ("<M-left>" . nil)
	      ("<M-right>" . nil)
	      ("<M-S-left>" . elpy-nav-indent-shift-left)
	      ("<M-S-right>" . elpy-nav-indent-shift-right)
	      ("C-c i" . elpy-autopep8-fix-code)
	      ("C-c C-d" . elpy-doc)))

(use-package pip-requirements
  :hook ((pip-requirements-mode . #'pip-requirements-auto-complete-setup )))

(use-package py-autopep8
  :hook ((python-mode . py-autopep8-enable-on-save)))

(provide 'lang-python)
;;; lang-python.el ends here
