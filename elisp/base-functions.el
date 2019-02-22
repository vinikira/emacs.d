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

(defun vs/format-standardjs-buffer (&optional begin end)
  "Formart js buffer according standardjs, BEGIN region and END region."
  (interactive "r")
  (let ((temp-point (point)))
    (when (executable-find "standard")
      (call-shell-region
       (if (region-active-p) begin (point-min))
       (if (region-active-p) end (point-max))
       "standard --stdin --fix"
       t
       (current-buffer)))
    (forward-line 1)
    (ignore-errors
    (when (and (search-forward "standard:") (not (beginning-of-line)))
      (delete-region (point) (point-max))))
    (goto-char temp-point)))

(defun vs/format-xml-buffer (&optional begin end)
  "Format xml buffer using xmllint, BEGIN region and END region."
  (interactive "r")
  (when (executable-find "xmllint")
    (let ((curr-point (point)))
      (call-shell-region
       (if (region-active-p) begin (point-min))
       (if (region-active-p) end (point-max))
       "xmllint --format -"
       t
       (current-buffer))
      (goto-char curr-point))))

(defun vs/generate-nodejs-project-config-files ()
  "Generate indium generic config file for nodejs projects."
  (interactive)
  (when (string= major-mode "dired-mode")
    (shell-command
     (format "echo '{\"configurations\": [{\"name\": \"%s\",\"type\": \"%s\",\"command\": \"%s\"}]' > .indium.json"
             (read-string "Enter indium project name:")
             (read-string "Enter indium project type (node or chrome):")
             (read-string "Enter indium command:")))))

;; Scratch Buffers stuff

(defun vs/generic-scratch-buffer (new-buffer-name mode &optional open-new-frame)
  "Open generic scratch buffer"
  (when open-new-frame
    (select-frame
     (new-frame)))
  (switch-to-buffer
   (get-buffer-create new-buffer-name))
  (funcall mode))

(defun vs/scratch-restclient (open-new-frame)
  "Create a new restclient scratch buffer."
  (interactive "P")
  (vs/generic-scratch-buffer "*restclient-scratch*" 'restclient-mode open-new-frame))

(defun vs/scratch-js (open-new-frame)
  "Create a new javascript scratch buffer."
  (interactive "P")
  (vs/generic-scratch-buffer "*js-scratch*" 'js2-mode open-new-frame))

(defun vs/scratch-json (open-new-frame)
  "Create a new json scratch buffer."
  (interactive "P")
  (vs/generic-scratch-buffer "*json-scratch*" 'json-mode open-new-frame))

(defun vs/scratch-xml (open-new-frame)
  "Create a new xml scratch buffer."
  (interactive "P")
  (vs/generic-scratch-buffer "*xml-scratch*" 'xml-mode open-new-frame))

(provide 'base-functions)
;;; base-functions ends here
