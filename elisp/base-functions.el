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

(defun eslint-find-binary ()
  (or
    (let ((root (locate-dominating-file buffer-file-name "node_modules")))
      (if root
          (let ((eslint (concat root "node_modules/.bin/eslint")))
            (if (file-executable-p eslint) eslint))))
    (error "Couldn't find a eslint executable. Please run command: \"sudo npm i eslint --save-dev\"")))

(defun eslint-fix-file ()
  "Format the current file with ESLint."
  (interactive)
        (progn (call-process
                (eslint-find-binary)
                nil nil nil
                buffer-file-name "--fix")
(revert-buffer t t t)))

(provide 'base-functions)
