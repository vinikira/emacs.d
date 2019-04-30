;;; base-theme --- Custom themes
;;; Commentary:
;;; Code:

(defvar vs/chosen-theme 'doom-dracula)
(defvar vs/chosen-font-name "xos4 Terminus")
(defvar vs/chosen-font-size 12)
(defvar vs/theme-loaded nil)

(defun vs/load-my-theme-config (&optional frame)
  "Load config for current FRAME."
  (when frame (select-frame frame))
  (unless vs/theme-loaded (load-theme vs/chosen-theme t))
  (when (member vs/chosen-font-name (font-family-list))
    (add-to-list 'default-frame-alist `(font . ,(format "%s-%d" vs/chosen-font-name vs/chosen-font-size)))
    (set-face-attribute 'default nil :font (format "%s-%d" vs/chosen-font-name vs/chosen-font-size))
    (set-frame-font (format "%s-%d" vs/chosen-font-name vs/chosen-font-size) nil t))
  (setq vs/theme-loaded t))

(use-package doom-themes
  :ensure
  :init (setq doom-themes-enable-bold t
              doom-themes-enable-italic t)
  :config (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(add-hook 'after-make-frame-functions #'vs/load-my-theme-config)
(add-hook 'after-init-hook #'vs/load-my-theme-config)

(provide 'base-theme)
;;; base-theme.el ends here
