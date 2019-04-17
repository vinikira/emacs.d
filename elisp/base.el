;; base.el --- base settings
;;; Commentary:
;;; Code:

;; Speed up a little emacs loadtime when reduce the number of GC executions
;; during init evaluation and then reset to default value
(setq gc-cons-threshold (* 64 1024 1024)
      gc-cons-percentage 0.8)

(add-hook 'after-init-hook '(lambda ()
                              (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))
                                    gc-cons-percentage (car (get 'gc-cons-percentage 'standard-value)))))

(package-initialize)
(setq package-archives (append package-archives
			       '(("melpa" . "https://melpa.org/packages/")
			       ("elpy" . "http://jorgenschaefer.github.io/packages/")
			       ("org" . "http://orgmode.org/elpa/"))))

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(defconst private-dir  (expand-file-name "private" user-emacs-directory))
(defconst temp-dir (format "%s/cache" private-dir)
  "Hostname-based elisp temp directories.")

;; Core settings
;; UTF-8 please
(set-charset-priority 'unicode)
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; please
(prefer-coding-system        'utf-8)   ; with sugar on top
(setq default-process-coding-system '(utf-8-unix . utf-8-unix)
      locale-coding-system          'utf-8)

;; Emacs customizations
(setq confirm-kill-emacs                  'y-or-n-p
      confirm-nonexistent-file-or-buffer  t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point                 t
      require-final-newline               t
      visible-bell                        nil
      ring-bell-function                  'ignore
      custom-file                         "~/.emacs.d/.custom.el"
      ;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
      minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)

      ;; Disable non selected window highlight
      cursor-in-non-selected-windows     nil
      highlight-nonselected-windows      nil
      ;; PATH
      exec-path                          (append exec-path '("/usr/local/bin/"))
      indent-tabs-mode                   nil
      inhibit-startup-message            t
      fringes-outside-margins            t
      select-enable-clipboard            t

      ;; Backups enabled, use nil to disable
      history-length                     1000
      backup-inhibited                   nil
      make-backup-files                  t
      auto-save-default                  t
      auto-save-list-file-name           (concat temp-dir "/autosave")
      make-backup-files                  t
      create-lockfiles                   nil
      backup-directory-alist            `((".*" . ,(concat temp-dir "/backup/")))
      auto-save-file-name-transforms    `((".*" ,(concat temp-dir "/backup/") t))

      ;; smooth scroling
      redisplay-dont-pause               t
      scroll-margin                      1
      scroll-step                        1
      scroll-conservatively              10000
      scroll-preserve-screen-position    nil

      ;; disable line wrap
      truncate-lines                     t

      ;; more memory
      gc-cons-threshold                  20000000)


(setq-default  ;; Bookmarks
	       ;; persistent bookmarks
	       bookmark-save-flag        t
	       bookmark-default-file     (concat temp-dir "/bookmarks"))

(fset 'yes-or-no-p 'y-or-n-p)

;; Enable modes
(mapc (lambda (it) (funcall it 1))
      '(global-auto-revert-mode
	  show-paren-mode))

;; Disable modes
(mapc (lambda (it) (funcall it -1))
      '(menu-bar-mode
	tool-bar-mode
	scroll-bar-mode))

;; Delight modes
(mapc (lambda (it) (funcall 'delight it))
      '(auto-revert-mode
	page-break-lines-mode
	eldoc-mode))

;; Hooks

;; linum only prog and text mode
(defun vs/line-numbers ()
  "Display line numbers."
  (if (fboundp 'global-display-line-numbers-mode)
  (funcall (lambda ()
    (display-line-numbers-mode 1)
    (hl-line-mode 1)))
  (linum-mode 1)))

(add-hook 'prog-mode-hook 'vs/line-numbers)
(add-hook 'text-mode-hook 'vs/line-numbers)

;; Delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Optimize large files
(add-hook 'find-file-hook
	  '(lambda ()
	     (when (> (buffer-size) (* 1024 1024))
	       (setq buffer-read-only t)
	       (linum-mode 0)
	       (buffer-disable-undo)
	       (fundamental-mode))))

(defun vs/font-lock ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|NOCOMMIT\\)"
	  1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'vs/font-lock)

;; Start Emacs server
(require 'server)
(when (and (fboundp 'server-running-p)
         (not (server-running-p)))
   (server-start))

(provide 'base)
;;; base ends here
