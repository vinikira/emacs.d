;;; base-extensions.el --- Base extensions
;;; Commentary:
;;; Code:

(use-package ace-window
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))
    ))

(use-package avy
  :bind
  ("C-c SPC" . avy-goto-char))

(use-package company
  :diminish 'company-mode
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0))

(use-package company-quickhelp
    :config
  (add-hook 'after-init-hook 'company-quickhelp-mode))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 5)
                        (projects . 5)
                        (agenda . 5))))

(use-package delight)

(use-package diminish)

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  (setq ediff-diff-options "-w"))

(use-package editorconfig
  :diminish 'editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package exec-path-from-shell
  :config
  ;; Add GOPATH to shell
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-copy-env "GOPATH")
    (exec-path-from-shell-copy-env "PYTHONPATH")
    (exec-path-from-shell-initialize)))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package flycheck
  :defer t
  :diminish
  :config
  ;; (defun my/use-eslint-from-node-modules ()
  ;; (let* ((root (locate-dominating-file
  ;;               (or (buffer-file-name) default-directory)
  ;;               "node_modules"))
  ;;        (eslint (and root
  ;;                     (expand-file-name "node_modules/.bin/eslint"
  ;;                                       root))))
  ;;   (when (and eslint (file-executable-p eslint))
  ;;     (setq-local flycheck-javascript-eslint-executable eslint))))
  ;; (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  (setq flycheck-javascript-eslint-executable "eslint_d")
  (global-flycheck-mode))

(use-package counsel
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-m" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x c k" . counsel-yank-pop))

(use-package counsel-projectile
  :bind
  ("C-x v" . counsel-projectile)
  ("C-x c p" . counsel-projectile-ag)
  :config
  (counsel-projectile-on))

(use-package ivy
  :diminish 'ivy-mode
  :bind
  ("C-x s" . swiper)
  ("C-x C-r" . ivy-resume)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers nil)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

(use-package hlinum
  :config
  (hlinum-activate))

(use-package linum
  :config
  (setq linum-format " %3d ")
  (global-linum-mode nil))

(use-package magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind
  ;; Magic
  ("C-x g s" . magit-status)
  ("C-x g x" . magit-checkout)
  ("C-x g c" . magit-commit)
  ("C-x g p" . magit-push)
  ("C-x g u" . magit-pull)
  ("C-x g e" . magit-ediff-resolve)
  ("C-x g r" . magit-rebase-interactive))

(use-package magit-popup)

(use-package markdown-mode
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode))

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C->" . mc/mark-all-like-this))

(use-package org
  :config
  (setq org-directory "~/Dropbox/org-files"
        org-default-notes-file (concat org-directory "/todo.org")
        org-agenda-files '("~/Dropbox/org-files")
        org-src-fontify-natively t
        org-log-done 'time
        org-capture-templates
        '(("t" "todo" entry (file org-default-notes-file)
           "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)
          ("m" "Meeting" entry (file org-default-notes-file)
           "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)
          ("d" "Diary" entry (file+datetree "~/org/diary.org")
           "* %?\n%U\n" :clock-in t :clock-resume t)
          ("i" "Idea" entry (file org-default-notes-file)
           "* %? :IDEA: \n%t" :clock-in t :clock-resume t)
          ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
           "** NEXT %? \nDEADLINE: %t") ))
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda))

(use-package org-projectile
  :bind (("C-c n p" . org-projectile-project-todo-completing-read)
         ("C-c c" . org-capture))
  :config
  (org-projectile-per-project)
  (setq org-projectile-projects-file "todo.org"
	org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

(use-package org-bullets
  :config
  (setq org-hide-leading-stars t)
  (add-hook 'org-mode-hook
            (lambda ()
              (org-bullets-mode t))))

(use-package page-break-lines)

;; (use-package powerline
;;   :init
;;   (powerline-default-theme))

(use-package projectile
  :delight '(:eval (concat " [" (projectile-project-name) "]"))
  :config
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" temp-dir))

  (setq projectile-completion-system 'ivy)

  (projectile-mode))

(use-package treemacs
  :config
  (progn
    (setq treemacs-git-mode t))
  :bind
  (:map global-map
        ([f8]        . treemacs)
        ("M-0"       . treemacs-select-window)
        ("C-c 1"     . treemacs-delete-other-windows)))

(use-package treemacs-projectile
  :after treemacs
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header))

(use-package try)

(use-package twittering-mode
  :config
  (setq twittering-icon-mode t
	twittering-use-master-password t))

(use-package recentf
  :config
  (setq recentf-save-file (recentf-expand-file-name "~/.emacs.d/private/cache/recentf"))
  (recentf-mode 1))

(use-package restclient
  :mode
  ("\\.http$" . restclient-mode)
  ("\\.https$" . restclient-mode))

(use-package restclient-test)

(use-package smartparens
  :delight
  :config (smartparens-global-mode))

(use-package smex)

(use-package undo-tree
  :diminish 'undo-tree-mode
  :config
  ;; Remember undo history
  (setq
   undo-tree-auto-save-history nil
   undo-tree-history-directory-alist `(("." . ,(concat temp-dir "/undo/"))))
  (global-undo-tree-mode 1))

(use-package which-key
  :diminish 'which-key-mode
  :config
  (which-key-mode))

(use-package windmove
  :bind
  ("C-x <up>" . windmove-up)
  ("C-x <down>" . windmove-down)
  ("C-x <left>" . windmove-left)
  ("C-x <right>" . windmove-right))

(use-package wgrep)

(use-package yasnippet
  :diminish 'yas-minor-mode
  :init (setq yas-snippet-dirs
              '("~/.emacs.d/snippets/"
                yas-installed-snippets-dir))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(provide 'base-extensions)
;;; base-extensions ends here
