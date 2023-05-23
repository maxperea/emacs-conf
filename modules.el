(use-package emacs
  :config
  ;; AUTH
  (setq auth-sources '("~/.authinfo"))

  (setq inhibit-startup-message t)
  (setq initial-scratch-message nil)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)

  (setq-default cursor-in-non-selected-windows nil)

  (setq
   make-backup-files nil
   auto-save-default nil
   create-lockfiles nil)

  (setq global-hl-line-mode nil)
  ;; (setq-default mode-line-format nil)
  (setq-default mode-line-format '("%e"
        			  mode-line-front-space
        			  ;; mode-line-mule-info
        			  mode-line-client
        			  mode-line-modified
        			  mode-line-remote
        			  mode-line-frame-identification
        			  mode-line-buffer-identification
        			  ;; mode-line-position
        			  (vc-mode vc-mode)
        			  ;; mode-line-modes
        			  ;; mode-line-misc-info
        			  mode-line-end-spaces))

  (setq debug-on-error nil)
  (setq-default ns-use-proxy-icon nil)
  (setq-default frame-title-format "%*%b")

  (add-to-list 'default-frame-alist '(font . "JetBrains Mono 14"))
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
  (setq read-process-output-max (* 1024 1024))
  (setq scroll-margin 8)
  (setq ring-bell-function #'ignore)
  (setq tab-always-indent 'complete)
  (setq-default show-trailing-whitespace t)

  (setq show-paren-delay 0)
  (show-paren-mode 1)

  (setq display-line-numbers-grow-only t)
  (setq display-line-numbers-width-start t)
  (setq display-line-numbers-type 'visual)
  (global-display-line-numbers-mode nil)

  (blink-cursor-mode -1)
  (setq-default indent-tabs-mode nil)
  (save-place-mode 1)
  (recentf-mode 1)
  (winner-mode 1)
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
	'(("t" "Personal Todo" entry
	   (file+headline org-capture-todo-file "Todos")
	   "* TODO %t %?\n%i\n" :prepend t)
	  ("l" "Personal Todo with Link" entry
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

(use-package magit
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (setq magit-display-buffer-function
	(lambda (buffer)
	  (display-buffer
	   buffer (if (and (derived-mode-p 'magit-mode)
			   (memq (with-current-buffer buffer major-mode)
				 '(magit-process-mode
				   magit-revision-mode
				   magit-diff-mode
				   magit-stash-mode
				   magit-status-mode)))
		      nil
		    '(display-buffer-same-window))))))

(use-package forge :after magit)

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
(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package doom-snippets
  :after yasnippet
  :straight (doom-snippets :type git
			   :host github
			   :repo "hlissner/doom-snippets"
			   :files ("*.el" "*")))

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
  ;; :straight
  ;; (:type built-in)
  :config
  (setq eglot-events-buffer-size 0)
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider))
  (setq eglot-confirm-server-initiated-edits nil)
  ;; (setq eglot-extend-to-xref t)
  ;; (setq-default eglot-inlay-hints-mode nil) ;; Does not seem to work
  ;; (setq eglot-send-changes-idle-time 2.0)
  ;; (setq eglot-confirm-server-initiated-edits nil)
  ;; (setq eldoc-echo-area-use-multiline-p 2)
  ;; (setq eldoc-echo-area-display-truncation-message nil)
  ;; (setq eglot-connect-timeout 2)
  ;; (setq eglot-sync-connect 0)
  ;; TODO: Now manually editing 'eglot-highlight-smybol-face' to
  ;; use 'secondary-selection', so that highlights actually are visible.
  ;; TODO: Now manually removed calls to 'eglot--signal-textDocument/didSave'
  ;; :hook ((before-save . eglot-format-buffer))
  ) ;; TODO: only do in eglot mode

;; Navigation
(use-package consult
  :config
  ;; (consult-customize consult--source-buffer :hidden t :default nil)
  ;; (add-to-list 'consult-buffer-sources persp-consult-source)
  :bind ("C-x b" . consult-buffer))

(use-package vertico
  :custom
  (vertico-count 15)
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

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode))

(use-package savehist
  :config
  (savehist-mode))

;; Completion
(use-package corfu
  :config
  (setq corfu-auto t
	corfu-auto-delay 0.1
	corfu-auto-prefix 1)
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

;;Themes
;; (use-package material-theme)
;; (use-package timu-spacegrey-theme
;;   :straight
;;   (timu-spacegrey-theme :type git :host github :repo "maxperea/timu-spacegrey-theme"))
;; (use-package darktooth-theme)
;; (use-package cyberpunk-theme)
;; (use-package kaolin-themes)
;; (use-package sublime-themes)
;; (use-package subatomic-theme)
;; (use-package gruvbox-theme
;;   :config
  ;; (setq doom-themes-enable-italic nil))

(use-package doom-themes
:config
  (setq doom-themes-enable-italic nil))
(load-theme 'doom-gruvbox t)
;; ---

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

;; TEST
(use-package avy)

;; (use-package copilot
;;   :config
;;   (defun my/copilot-tab ()
;;     (interactive)
;;     (or (copilot-accept-completion)
;; 	(indent-for-tab-command)))

;;   (with-eval-after-load 'copilot
;;     (general-def 'insert copilot-mode-map
;;       "<tab>" #'my/copilot-tab))
;;   :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
;;   :ensure t)

(use-package perspective
  :init
  (setq persp-suppress-no-prefix-key-warning t)
  :config
  (setq persp-state-default-file (expand-file-name "persp-state-save.data" config-base-directory))
  (persp-mode))

;; (use-package mood-line
;;   :config
;;   (mood-line-mode)
;;   (setq mood-line-evil-state-alist nil)
;;   (setq-default mode-line-format
;;                 '((:eval
;;                    (mood-line--format
;;                     ;; Left
;;                     (format-mode-line
;;                      '(" "
;;                        (:eval (mood-line-segment-modal))
;;                        (:eval (mood-line-segment-buffer-status))
;;                        (:eval (mood-line-segment-buffer-name))
;;                        (:eval (mood-line-segment-anzu))
;;                        (:eval (mood-line-segment-multiple-cursors))
;;                        (:eval (mood-line-segment-cursor-position))
;;                        ))

;;                     ;; Right
;;                     (format-mode-line
;;                      '(
;;                        ;; (:eval (mood-line-segment-indentation))
;;                        ;; (:eval (mood-line-segment-eol))
;;                        ;; (:eval (mood-line-segment-encoding))
;;                        (:eval (mood-line-segment-vc))
;;                        ;; (:eval (mood-line-segment-major-mode))
;;                        ;; (:eval (mood-line-segment-misc-info))
;;                        (:eval (mood-line-segment-checker))
;;                        ;; (:eval (mood-line-segment-process))
;;                        " ")))))))
