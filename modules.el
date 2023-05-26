(use-package emacs
  :config
  (add-to-list 'default-frame-alist '(font . "JetBrains Mono 14" ))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist
               '(vertical-scroll-bars . nil))

  (add-to-list 'auto-mode-alist '("\\.njk\\'" . web-mode))


  (setq-default
   shr-max-width 80
   indent-tabs-mode nil
   ns-use-proxy-icon nil
   frame-title-format "%*%b"
   cursor-in-non-selected-windows nil
   truncate-lines t
   show-trailing-whitespace nil)

  (setq
   inhibit-startup-message t
   initial-scratch-message nil
   make-backup-files nil
   auto-save-default nil
   create-lockfiles nil
   gc-cons-threshold (* 100 1024 1024)
   read-process-output-max (* 1024 1024)
   scroll-margin 8
   ring-bell-function #'ignore
   tab-always-indent 'complete
   show-paren-delay 0
   display-line-numbers-grow-only t
   display-line-numbers-width-start t
   display-line-numbers-type 'visual)

  (toggle-scroll-bar -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (fset 'yes-or-no-p 'y-or-n-p)
  (show-paren-mode 1)
  (global-display-line-numbers-mode nil)
  (blink-cursor-mode -1)
  (save-place-mode 1)
  (recentf-mode 1)
  (winner-mode 1)
  (global-subword-mode 1)

  (setq-default
   mode-line-format
   '("%e"
     mode-line-front-space
     mode-line-client
     mode-line-modified
     mode-line-remote
     mode-line-frame-identification
     mode-line-buffer-identification
     (vc-mode vc-mode)
     mode-line-end-spaces))

  (add-hook 'compilation-mode-hook
            (lambda () (visual-line-mode 1)))

  (add-hook 'compilation-minor-mode-hook
            (lambda () (visual-line-mode 1)))

  :hook
  (text-mode . auto-fill-mode)
  (before-save . delete-trailing-whitespace))

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
  (with-eval-after-load 'magit
    (define-key magit-mode-map (kbd "<SPC>") nil))
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
  (setq undo-tree-history-directory-alist
        '(("." . "~/.emacs.d/undo")))
  (global-undo-tree-mode))

(use-package format-all)

(use-package which-key
  :config
  (which-key-mode))

(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key))

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
  :custom-face
  (eglot-highlight-symbol-face ((t (:inherit secondary-selection))))
  :config
  (setq eglot-events-buffer-size 0)
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider))
  (setq eglot-confirm-server-initiated-edits nil))

(use-package consult)

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

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode))

(use-package savehist
  :config
  (savehist-mode))

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

(use-package doom-themes
  :config
  (setq doom-themes-enable-italic nil)
  (load-theme 'doom-gruvbox t))

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

(use-package avy)

(use-package perspective
  :init
  (setq persp-suppress-no-prefix-key-warning t)
  :config
  (persp-mode))
