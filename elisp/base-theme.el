;;; base-theme --- Custom themes
;;; Commentary:
;;; Code:

(defun vs/config-doom-theme ()
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(defun vs/load-frame-theme (frame)
	  "FRAME."
	  (select-frame frame)
	  (vs/config-doom-theme))

(use-package doom-themes
  :ensure t
  :init (setq doom-themes-enable-bold t
              doom-themes-enable-italic t
              doom-neotree-file-icons t)
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions #'vs/load-frame-theme)
    (vs/config-doom-theme)))

(provide 'base-theme)
;;; base-theme ends here
