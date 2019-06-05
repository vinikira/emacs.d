;;; base-global-keys.el --- Custom keybindings
;;; Commentary:
;;; Add your keys here, as such
;;; (global-set-key (kbd "[SHORTCUT]") '[FUNCTION])
;;; Code:

(global-set-key (kbd "C-x 2") 'vs/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'vs/split-window-right-and-switch)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Scratch buffer
(global-set-key (kbd "C-c s b") 'vs/scratch-buffer)

(global-set-key (kbd "M-<down>") 'enlarge-window)
(global-set-key (kbd "M-<up>") 'shrink-window)
(global-set-key (kbd "M-<left>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<right>") 'shrink-window-horizontally)

;; Indent all bufer
(global-set-key (kbd "C-c i") 'vs/indent-buffer)

;; Call shell
(global-set-key (kbd "C-x C-z") 'shell)

;; Remap search
(global-set-key (kbd "C-x s") 'isearch-forward)

(provide 'base-global-keys)
;;; base-global-keys ends here
