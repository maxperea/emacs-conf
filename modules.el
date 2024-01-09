(use-package emacs
  :config
  (set-frame-parameter nil 'undecorated t)

  (mapc
   (lambda (item) (add-to-list 'default-frame-alist item))
   '((font . "JetBrains Mono 14")
     (ns-transparent-titlebar . t)
     (ns-appearance . dark)
     (vertical-scroll-bars . nil)))

  (setq-default
   tab-width 4
   frame-resize-pixelwise t
   shr-max-width 80
   indent-tabs-mode nil
   ns-use-proxy-icon nil
   frame-title-format "%*%b"
   cursor-in-non-selected-windows nil
   truncate-lines t
   show-trailing-whitespace nil)

  (setq
   vc-follow-symlinks t
   inhibit-startup-message t
   initial-scratch-message nil
   make-backup-files nil
   auto-save-default nil
   create-lockfiles nil
   gc-cons-threshold (* 100 1024 1024)
   read-process-output-max (* 1024 1024)
   scroll-margin 4
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

  ;; (setq-default mode-line-format nil)

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
  (setq org-todo-keywords '((sequence "TODO" "|" "DONE")
                            (sequence "TASK" "|" "DONE")
                            ))
  (setq org-capture-templates
        '(
          ("t" "Personal Todo" entry
           (file+headline org-capture-todo-file "Todos")
           "* TODO %t %?\n%i\n" :prepend t)

          ("T" "Personal Todo with Link" entry
           (file+headline org-capture-todo-file "Todos")
           "* TODO %t %?\n%i\n%a" :prepend t)

          ("s" "Personal Task" entry
           (file+headline org-capture-todo-file "Todos")
           "* TASK %?\n%i\n" :prepend t)

          ("S" "Personal Task with Link" entry
           (file+headline org-capture-todo-file "Todos")
           "* TASK %?\n%i\n%a" :prepend t)

          ("n" "Personal Note" entry
           (file+headline org-default-notes-file "Inbox")
           "* %u %?\n%i\n" :prepend t)

          ("N" "Personal Note with Link" entry
           (file+headline org-default-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t)

          ("j" "Journal" entry
           (file+olp+datetree org-capture-journal-file)
           "* %U %?\n%i\n%a" :prepend t)))
  (setq org-agenda-files
        (list org-default-notes-file
              org-capture-journal-file
              org-capture-todo-file)))


(use-package tree-sitter
  :defer
  :config
  (setq treesit-language-source-alist
        '(
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          ))
  :if (executable-find "tree-sitter")
  :hook (((
           c++-mode
           rustic-mode
           css-mode
           ) . tree-sitter-mode)
         ((
           c++-mode
           rustic-mode
           ) . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :defer
  :if (executable-find "tree-sitter")
  :after tree-sitter)

(use-package general)

(use-package magit
  :defer
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

(use-package git-timemachine
  :defer)

(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
        '(("." . "~/.emacs.d/undo")))
  (global-undo-tree-mode))

(use-package format-all
  :defer)

(use-package which-key
  :config
  (which-key-mode))

(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key))

(use-package yasnippet
  :defer
  :config
  (yas-global-mode 1))

;; (use-package doom-modeline
;;   :init
;;   (doom-modeline-mode))

(use-package doom-snippets
  :defer
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
  :defer
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
  :defer
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
        '(
          ("TASK"   . "#AAAAFF")
          ("TODO"   . "#FFD700")
          ("DONE"   . "#00FF00")
          ("UPDATE" . "#FFAAAA")
          ("NOTE"   . "#7777FF")
          ("FIXME"  . "#FF0000")))
  (global-hl-todo-mode))

(use-package avy)
(use-package treemacs)

;; LSP
(use-package eglot :straight (:type built-in)
  :custom-face
  (eglot-highlight-symbol-face ((t (:inherit secondary-selection))))
  :config
  (add-to-list 'eglot-server-programs
               '(angelscript-mode . ("angelscript-ls" "--stdio"))
               '(angel-cpp-mode . ("angelscript-ls" "--stdio"))
               )
  (setq eglot-events-buffer-size 0
        ;; eglot-extend-to-xref t
        ;; eglot-ignored-server-capabilities '(:inlayHintProvider)
        eglot-confirm-server-initiated-edits nil))

(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

(use-package dap-mode
  :ensure
  :config
   (setq dap-cpptools-extension-version "1.5.1")
   (setq dap-lldb-debug-program `(,(expand-file-name "/Users/maxp/.vscode/extensions/lanza.lldb-vscode-0.2.3/bin/darwin/bin/lldb-vscode")))

   (require 'dap-cpptools)

  (with-eval-after-load 'dap-cpptools
    ;; Add a template specific for debugging Rust programs.
    ;; It is used for new projects, where I can M-x dap-edit-debug-template
    (dap-register-debug-template "MY CONF"
                                 (list :type "lldb-mi"
                                       :request "launch"
                                       :name "Rust::Run"
                                       :MIMode "lldb"
                                       :miDebuggerPath "rust-lldb"
                                       ;; :environment []
                                       ;; :program "${workspaceFolder}/target/debug/seahorse_rnd"
                                       :cwd nil
                                       ;; :console "external"
                                       ;; :dap-compilation "cargo build"
                                       ;; :dap-compilation-dir "${workspaceFolder}"
                                       ))
    (dap-register-debug-template "Rust::CppTools Run Configuration"
                                 (list :type "cppdbg"
                                       :request "launch"
                                       :name "Rust::Run"
                                       :MIMode "lldb"
                                       :miDebuggerPath "rust-lldb"
                                       :environment []
                                       ;; :program "${workspaceFolder}/target/debug/seahorse_rnd"
                                       :cwd nil
                                       :console "external"
                                       :dap-compilation "cargo build"
                                       :dap-compilation-dir "${workspaceFolder}")))

  (with-eval-after-load 'dap-mode
    (setq dap-default-terminal-kind "integrated") ;; Make sure that terminal programs open a term for I/O in an Emacs buffer
    (dap-auto-configure-mode +1))

  (dap-ui-mode)
  (dap-ui-controls-mode 1)

  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup)
  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
         :request "launch"
         :name "LLDB::Run"
	 :gdbpath "rust-lldb"
         :target nil
         :cwd nil)))

(use-package impatient-mode)
