;;; base-theme --- Custom themes
;;; Commentary:
;;; Code:

(defun vs/load-theme (frame)
	  "FRAME."
	  (select-frame frame)
	  (load-theme 'kaolin-dark t))

(use-package kaolin-themes
  :ensure t
  :init
  (if (daemonp)
      (add-hook 'after-make-frame-functions #'vs/load-theme)
    (load-theme 'kaolin-dark t)))

(provide 'base-theme)
;;; base-theme ends here
