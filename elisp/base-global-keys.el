;;; base-global-keys.el --- Custom keybindings
;;; Commentary:
;;; Add your keys here, as such
;;; (global-set-key (kbd "[SHORTCUT]") '[FUNCTION])
;;; Code:

(global-set-key (kbd "C-x 2") 'vs/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'vs/split-window-right-and-switch)

;; Scratch buffers stuff
(global-set-key (kbd "C-c s b r") 'vs/scratch-restclient)
(global-set-key (kbd "C-c s b e") 'vs/scratch-js)
(global-set-key (kbd "C-c s b j") 'vs/scratch-json)
(global-set-key (kbd "C-c s b x") 'vs/scratch-xml)

(global-set-key (kbd "M-<down>") 'enlarge-window)
(global-set-key (kbd "M-<up>") 'shrink-window)
(global-set-key (kbd "M-<left>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<right>") 'shrink-window-horizontally)

(provide 'base-global-keys)
;;; base-global-keys ends here
