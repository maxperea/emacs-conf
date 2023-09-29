(use-package yaml-mode
  :defer)
(use-package clang-format
  :defer
  :hook
  (c++-ts-mode . (lambda ()
                   (add-hook 'before-save-hook 'clang-format-buffer nil 'make-it-local)))
  :config
  (setq-default clang-format-style "google"))
(use-package cmake-mode
  :defer)
(use-package markdown-mode
  :defer)
(use-package lua-mode
  :defer)
(use-package erlang
  :defer)
(use-package haskell-mode
  :defer)
(use-package typescript-mode
  :defer)
(use-package flutter
  :defer)
(use-package swift-mode
  :defer)
(use-package docker
  :defer)
(use-package dockerfile-mode
  :defer)
(use-package nix-mode
  :defer)
(use-package web-mode
  :defer
  :config
  (add-to-list 'auto-mode-alist '("\\.njk\\'" . web-mode)))
(use-package rustic
  :defer
  :hook
  (rustic-mode . (lambda ()
                   (add-hook 'before-save-hook 'eglot-format-buffer nil 'make-it-local)))
  :config
  (setq rustic-cargo-bin-remote "/usr/local/cargo/bin/cargo")
  (setq rustic-lsp-client 'eglot))
(use-package dart-mode
  :defer
  :hook
  (dart-mode . (lambda ()
                 (add-hook 'before-save-hook 'eglot-format-buffer nil 'make-it-local)))
  (dart-mode . (lambda ()
                 (setq-default eglot-workspace-configuration
                               '((:dart . (:completeFunctionCalls t  :applyEdit t)))))))
