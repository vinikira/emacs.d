;; Add your custom functions here

;; (defun something
;;    (do-something))

(defun split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun close-all-buffers ()
  "Close all buffers in list."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(provide 'base-functions)
