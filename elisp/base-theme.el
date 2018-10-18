;;; base-theme --- Custom themes
;;; Commentary:
;;; Code:

(defun vs/load-theme (frame)
	  "FRAME."
	  (select-frame frame)
	  (load-theme 'dracula t))

(use-package dracula-theme
  :ensure t
  :init
  (if (daemonp)
      (add-hook 'after-make-frame-functions #'vs/load-theme)
    (load-theme 'dracula t)))

(provide 'base-theme)
;;; base-theme ends here
