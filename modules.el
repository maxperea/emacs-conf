(use-package emacs
  :config
  (setq inhibit-startup-message t)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)

  (add-to-list 'default-frame-alist '(font . "JetBrains Mono 15"))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist
	       '(vertical-scroll-bars . nil))
  (set-default 'truncate-lines t)
  (fset 'yes-or-no-p 'y-or-n-p)

  (add-hook 'text-mode-hook 'auto-fill-mode)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (setq backup-directory-alist `(("." . "~/.emacs_saves")))
  (setq gc-cons-threshold (* 100 1024 1024))
  (setq scroll-margin 8)
  (setq ring-bell-function #'ignore)
  (setq tab-always-indent 'complete)
  (setq show-trailing-whitespace t)

  (setq show-paren-delay 0)
  (show-paren-mode 1)

  (setq display-line-numbers-grow-only t)
  (setq display-line-numbers-width-start t)
  (setq display-line-numbers-type 'visual)
  (global-display-line-numbers-mode 1)

  (blink-cursor-mode -1)
  (indent-tabs-mode nil)
  (save-place-mode 1)
  (recentf-mode 1)
  (winner-mode 1)
  (eldoc-mode 1)
  (global-subword-mode 1))

(use-package org
  :straight
  (:type built-in)
  :config
  (setq org-agenda-window-setup 'current-window)
  (setq org-log-done 'time)
  (setq org-directory "~/org")
  (setq org-default-notes-file (expand-file-name "notes.org" org-directory))
  (setq org-capture-journal-file (expand-file-name "journal.org" org-directory))
  (setq org-capture-todo-file (expand-file-name "todo.org" org-directory))
  (setq org-capture-templates
	'(("l" "Personal Todo" entry
	   (file+headline org-capture-todo-file "Todos")
	   "* TODO %t %?\n%i\n%a" :prepend t)
	  ("n" "Personal Note" entry
	   (file+headline org-default-notes-file "Inbox")
	   "* %u %?\n%i\n%a" :prepend t)
	  ("j" "Journal" entry
	   (file+olp+datetree org-capture-journal-file)
	   "* %U %?\n%i\n%a" :prepend t)))
  (setq org-agenda-files
	(list org-default-notes-file
	      org-capture-journal-file
	      org-capture-todo-file)))

(use-package general)

(use-package magit)

(use-package git-timemachine)

(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (global-undo-tree-mode))

(use-package which-key
  :config
  (which-key-mode))

(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key))

;; Code Intelligence
(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (sp-with-modes
      '(dart-mode rust-mode typescript-mode)
    (sp-local-pair "[" nil :post-handlers '(:add ("||\n[i]" "RET")))
    (sp-local-pair "(" nil :post-handlers '(:add ("||\n[i]" "RET")))
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))

(use-package eglot
  :config
  (setq eldoc-echo-area-use-multiline-p 2)
  (setq eldoc-echo-area-display-truncation-message nil)
  (setq eglot-confirm-server-initiated-edits nil)
  (setq eglot-extend-to-xref t)
  ;; TODO: Now manually editing 'eglot-highlight-smybol-face' to
  ;; use 'secondary-selection', so that highlights actually are visible.
  ;; TODO: Now manually removed calls go 'eglot--signal-textDocument/didSave' 
  :hook ((before-save . eglot-format-buffer))) ;; TODO: only do in eglot mode

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package doom-snippets
  :after yasnippet
  :straight (doom-snippets :type git
			   :host github
			   :repo "hlissner/doom-snippets"
			   :files ("*.el" "*")))

;; Navigation
(use-package consult
  :bind ("C-x b" . consult-buffer))

(use-package vertico
  :config
  (vertico-mode 1)
  :straight ( vertico :files (:defaults "extensions/*")
	      :includes (vertico-directory)))

(use-package vertico-directory
  :after vertico
  :straight nil
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :config
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :config
  (savehist-mode))

;; Completion
(use-package corfu
  :config
  (setq corfu-auto t
	corfu-auto-delay 0.1
	corfu-auto-prefix 0)
  (add-hook 'eshell-mode-hook
	    (lambda ()
	      (setq-local corfu-auto nil)
	      (corfu-mode)))
  :init
  (global-corfu-mode)
  :straight ( corfu :files (:defaults "extensions/*")
	      :includes (corfu-popupinfo)))

(use-package corfu-popupinfo
  :after corfu
  :config
  (setq corfu-popupinfo-delay 0.2)
  :init
  (corfu-popupinfo-mode))

;; Look & Feel
(use-package timu-spacegrey-theme
  :straight
  (timu-spacegrey-theme :type git :host github :repo "maxperea/timu-spacegrey-theme")
  :config
  (load-theme 'timu-spacegrey t))

(use-package nano-modeline
  :config
  (setq nano-modeline-prefix 'default)
  (defun nano-modeline-vc-branch () nil)
  (setq nano-modeline-position 'bottom)
  (nano-modeline-mode))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces
	'(("TODO"   . "#FFD700")
	  ("DONE"   . "#00FF00")
	  ("FIXME"  . "#FF0000")))
  (global-hl-todo-mode))
