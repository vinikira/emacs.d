;;; base-functions --- Custom functions
;;; Commentary:
;;; Add your custom functions here
;;; (defun something
;;;    (do-something))
;;; Code:

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

(defun setup-tide-mode ()
  "Setup typscript mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/.bin/eslint"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)


(provide 'base-functions)
;;; base-functions ends here
