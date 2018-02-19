;;; base-global-keys.el --- Custom keybindings
;;; Commentary:
;;; Add your keys here, as such
;;; (global-set-key (kbd "[SHORTCUT]") '[FUNCTION])
;;; Code:

(global-set-key (kbd "C-x 2") 'split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'split-window-right-and-switch)

(provide 'base-global-keys)
;;; base-global-keys ends here
