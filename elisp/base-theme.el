;;; base-theme --- Custom themes
;;; Commentary:
;;; Code:

(defvar vs/chosen-theme 'doom-dracula)
(defvar vs/chosen-font-name "Terminus (TTF)")
(defvar vs/chosen-font-size 12)

(defun vs/load-my-theme-config (&optional frame)
  "Load config for current FRAME."
  (when frame (select-frame frame))
  (load-theme vs/chosen-theme t)
  (when (member vs/chosen-font-name (font-family-list))
    (add-to-list 'default-frame-alist `(font . ,(format "%s-%d" vs/chosen-font-name vs/chosen-font-size)))
    (set-face-attribute 'default nil :font (format "%s-%d" vs/chosen-font-name vs/chosen-font-size))
    (set-frame-font (format "%s-%d" vs/chosen-font-name vs/chosen-font-size) nil t)))

(use-package doom-themes
  :ensure t
  :init (setq doom-themes-enable-bold t
              doom-themes-enable-italic t
	      doom-neotree-file-icons t)
  :config (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(add-hook 'after-make-frame-functions #'vs/load-my-theme-config)
(add-hook 'after-init-hook #'vs/load-my-theme-config)

(provide 'base-theme)
;;; base-theme.el ends here
