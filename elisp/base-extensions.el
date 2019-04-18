;;; base-extensions.el --- Base extensions
;;; Commentary:
;;; Code:

(use-package ace-window
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))))

(use-package avy
  :bind
  ("C-c SPC" . avy-goto-char))

(use-package all-the-icons
  :config (when (memq window-system '(ns))
            (setq inhibit-compacting-font-caches t)))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq-default company-dabbrev-downcase 0)
  (setq company-idle-delay 0))

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

(use-package ediff
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  (setq ediff-diff-options "-w"))

(use-package editorconfig
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

(use-package fancy-narrow
  :config (fancy-narrow-mode))

(use-package flycheck
  :init
  (setq flycheck-javascript-eslint-executable "eslint_d")
  (global-flycheck-mode 1))

(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :config (global-git-gutter-mode))

(use-package counsel
  :config (counsel-mode 1)
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x c k" . counsel-yank-pop)
  ("<f1> f" . counsel-describe-function)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> l" . counsel-load-library)
  ("<f2> i" . counsel-info-lookup-symbol)
  ("<f2> u" . counsel-unicode-char)
  ("C-x C-r" . counsel-recentf))

(use-package counsel-projectile
  :bind
  ("C-x v" . counsel-projectile)
  ("C-x c p" . counsel-projectile-ag))

(use-package ivy
  :bind
  ("C-x s" . swiper)
  ("C-x C-r" . ivy-resume)
  ("C-x b" . ivy-switch-buffer)
  :config
  (setq ivy-use-virtual-buffers t)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :config (ivy-rich-mode 1))

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
  :mode ("\\.\\(m\\(ark\\)?down\\|md\\)$" . markdown-mode)
  :bind
  (:map markdown-mode
        ("C-c C-f" . markdownfmt-format-buffer)))

(use-package markdownfmt
  :hook (markdown-mode . markdownfmt-enable-on-save))

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("M-n" . mc/mark-next-like-this)
  ("M-p" . mc/mark-previous-like-this)
  ("C-c x" . mc/mark-all-like-this))

(use-package ob-restclient)

(use-package ob-ipython)

(use-package ob-async
  :config (setq ob-async-no-async-languages-alist '("ipython")))

(use-package org
  :config
  (setq org-directory (if (file-directory-p "~/Sync/org")
 			  "~/Sync/org"
 			"~/")
	org-default-notes-file (concat org-directory "/notes.org")
        org-agenda-files (list (concat org-directory "/work.org")
                               (concat org-directory "/personal.org"))
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
	     (sql . t)
	     (ipython . t))))
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  :hook (org-mode . (lambda () (display-line-numbers-mode -1)))
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
  :hook ((org-mode . org-bullets-mode))
  :config
  (setq org-hide-leading-stars t))

(use-package ox-reveal
  :init (setq org-reveal-root "https://cdn.jsdelivr.net/reveal.js/latest"
              org-reveal-mathjax t))

(use-package telephone-line
  :config
  (telephone-line-mode))

(use-package projectile
  :init
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" temp-dir)
        projectile-completion-system 'ivy
        projectile-globally-ignored-directories '("node_modules" ".git" ".svn"))
  :config (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package projectile-ripgrep
  :after projectile
  :bind (("C-c p r g" . projectile-ripgrep)))

(use-package treemacs
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ([f8]   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ([f7] . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-magit
  :after treemacs magit)

(use-package try
  :defer t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package recentf
  :config
  (setq recentf-save-file (recentf-expand-file-name "~/.emacs.d/private/cache/recentf"))
  (recentf-mode 1))

(use-package restclient
  :mode
  ("\\.http$" . restclient-mode)
  ("\\.https$" . restclient-mode))

(use-package restclient-test
  :after restclient-mode)

(use-package smartparens
  :config (smartparens-global-mode))

;; Commando history
(use-package smex)

(use-package undo-tree
  :config
  ;; Remember undo history
  (setq
   undo-tree-auto-save-history nil
   undo-tree-history-directory-alist `(("." . ,(concat temp-dir "/undo/"))))
  (global-undo-tree-mode 1))

(use-package which-key
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
  :init (setq yas-snippet-dirs
              '("~/.emacs.d/snippets/"))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after (yas-global-mode))

(provide 'base-extensions)
;;; base-extensions.el ends here
