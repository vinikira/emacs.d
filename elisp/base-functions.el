;;; base-functions --- Custom functions
;;; Commentary:
;;; Add your custom functions here
;;; (defun something
;;;    (do-something))
;;; Code:

(defun vs/split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun vs/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun vs/close-all-buffers ()
  "Close all buffers in list."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun vs/eslint-find-binary ()
  (or
   (let ((root (locate-dominating-file buffer-file-name "node_modules")))
     (if root
         (let ((eslint (concat root "node_modules/.bin/eslint")))
           (if (file-executable-p eslint) eslint))))
   (error "Couldn't find a eslint executable. Please run command: \"sudo npm i eslint --save-dev\"")))

(defun vs/eslint-fix-file ()
  "Format the current file with ESLint."
  (interactive)
  (progn (call-process
          (eslint-find-binary)
          nil nil nil
          buffer-file-name "--fix")
         (revert-buffer t t t)))

(defun vs/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/.bin/eslint"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'vs/use-eslint-from-node-modules)

(defun vs/format-standardjs-buffer ()
  "Formart js buffer according standardjs."
  (interactive)
  (save-buffer)
  (shell-command (format "standard --fix %s" (buffer-file-name))))

(provide 'base-functions)
;;; base-functions ends here
