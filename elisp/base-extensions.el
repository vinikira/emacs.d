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

(use-package all-the-icons
  :config (when (memq window-system '(ns))
            (setq inhibit-compacting-font-caches t)))

(use-package company
  :delight
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq-default company-dabbrev-downcase 0)
  (setq company-idle-delay 0))

(use-package company-lsp
  :delight
  :config (setq company-lsp-async t
                company-lsp-enable-snippet t))

(use-package company-quickhelp
  :if (fboundp 'company)
  :config
  (add-hook 'after-init-hook 'company-quickhelp-mode))

(use-package company-restclient
  :config (add-to-list 'company-backends 'company-restclient))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)
                          (agenda . 5))))

(use-package delight)

(use-package ediff
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  (setq ediff-diff-options "-w"))

(use-package editorconfig
  :delight
  :config
  (editorconfig-mode 1))

(use-package exec-path-from-shell
  :config
  ;; Add GOPATH to shell
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-copy-env "GOPATH")
    (exec-path-from-shell-copy-env "PYTHONPATH")
    (exec-path-from-shell-initialize)))

(use-package expand-region
  :bind
  ("C-c e r" . er/expand-region)
  ("C-c e p" . er/mark-inside-pairs))

(use-package flycheck
  :delight
  :init
  (setq flycheck-javascript-eslint-executable "eslint_d")
  (global-flycheck-mode 1))

(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :config (global-git-gutter-mode))

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
  :delight
  :bind
  ("C-x s" . swiper)
  ("C-x C-r" . ivy-resume)
  ("C-x b" . ivy-switch-buffer)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers nil)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

(use-package lsp-mode
  :hook ((lsp-mode . lsp-ui-mode)))

;; LSP UI
;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui)

(use-package magit
  :if (executable-find "git")
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

(use-package magit-popup
  :if (fboundp 'magit))

(use-package markdown-mode
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode))

(use-package markdownfmt
  :config
  (add-hook 'markdown-mode-hook #'markdownfmt-enable-on-save)
  :bind
  ("C-c C-f" . markdownfmt-format-buffer))

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("M-n" . mc/mark-next-like-this)
  ("M-p" . mc/mark-previous-like-this)
  ("C-c x" . mc/mark-all-like-this))

(use-package ob-restclient)

(use-package ob-async
  :config (setq ob-async-no-async-languages-alist '("ipython")))

(use-package org
  :config
  (setq org-directory (if (file-directory-p "~/Dropbox/org-files")
 			  "~/Dropbox/org-files"
 			"~/")
	org-default-notes-file (concat org-directory "/notes.org")
        org-agenda-files (list (concat org-directory "/work.org")
                             (concat org-directory "/school.org")
                             (concat org-directory "/home.org"))
        org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-log-done 'time
        org-babel-sh-command "bash"
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
           "** NEXT %? \nDEADLINE: %t")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages
	   '((emacs-lisp . t)
	     (python . t)
	     (restclient . t)
	     (js . t)
	     (shell . t)
	     (plantuml . t)
	     (sql . t))))
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

(use-package ox-reveal)

(use-package telephone-line
  :config
  (telephone-line-mode))

(use-package projectile
  :delight '(:eval (concat " [" (projectile-project-name) "]"))
  :bind (("C-c p s p" . projectile-switch-project)
         ("C-c p f f" . projectile-switch-project))
  :config
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" temp-dir)
        projectile-completion-system 'ivy
        projectile-globally-ignored-directories '("node_modules"))
  (projectile-global-mode))

(use-package neotree
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'nerd)
	neo-show-hidden-files t
	neo-smart-open t
	neo-confirm-delete-directory-recursively t
        neo-confirm-delete-file t
	neo-vc-integration '(face char)
	neo-auto-indent-point t
	)
  :bind
  (:map global-map
        ([f8] . neotree-toggle)
        ([f7] . neotree-find)))

(use-package try
  :defer t)

(use-package twittering-mode
  :defer t
  :config
  (setq twittering-icon-mode t
	twittering-use-master-password t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package recentf
  :config
  (setq recentf-save-file (recentf-expand-file-name "~/.emacs.d/private/cache/recentf"))
  (recentf-mode 1))

(use-package restclient
  :defer t
  :mode
  ("\\.http$" . restclient-mode)
  ("\\.https$" . restclient-mode))

(use-package restclient-test
  :defer t
  :if (fboundp 'restclient)
  :after (restclient-mode))

(use-package smartparens
  :delight
  :config (smartparens-global-mode))

;; Commando history
(use-package smex)

(use-package undo-tree
  :delight
  :config
  ;; Remember undo history
  (setq
   undo-tree-auto-save-history nil
   undo-tree-history-directory-alist `(("." . ,(concat temp-dir "/undo/"))))
  (global-undo-tree-mode 1))

(use-package which-key
  :delight
  :config
  (which-key-mode))

(use-package windmove
  :bind
  ("C-x <up>" . windmove-up)
  ("C-x <down>" . windmove-down)
  ("C-x <left>" . windmove-left)
  ("C-x <right>" . windmove-right))

(use-package wgrep
  :if (executable-find "grep"))

(use-package xclip
  :if (executable-find "xclip")
  :config (xclip-mode))

(use-package yasnippet
  :delight
  :init (setq yas-snippet-dirs
              '("~/.emacs.d/snippets/"))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :delight
  :after (yas-global-mode))

(provide 'base-extensions)
;;; base-extensions ends here
