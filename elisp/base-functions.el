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
       (current-buffer))
      (goto-char (point-min))
      (when (search-forward "standard:" nil t)
        (beginning-of-line)
        (delete-region (point) (point-max)))
      (goto-char temp-point))))

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

(defun vs/generate-indium-config-file ()
  "Generate indium generic config file for nodejs projects."
  (interactive)
  (when (string= major-mode "dired-mode")
    (shell-command
     (format "echo '{\"configurations\": [{\"name\": \"%s\",\"type\": \"%s\",\"command\": \"%s\"}]}' > .indium.json"
             (read-string "Enter indium project name:")
             (read-string "Enter indium project type (node or chrome):")
             (read-string "Enter indium command:")))))

;; Scratch Buffers stuff
(defun vs/scratch-buffer (open-new-frame)
  "Open generic scratch buffer"
  (interactive "P")
  (let ((selected-mode (completing-read
                        "Scratch buffer with mode: "
                        '("restclient-mode"
                          "js2-mode"
                          "json-mode"
                          "xml-mode"
                          "org-mode"
                          "sql-mode"
                          "lisp-interaction-mode"))))
    (when open-new-frame
      (select-frame
       (make-frame)))
    (switch-to-buffer
     (get-buffer-create (concat "*" selected-mode "*")))
    (funcall (intern selected-mode))))

;; Edit files with sudo
(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Indent buffer
(defun vs/indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(provide 'base-functions)
;;; base-functions ends here
