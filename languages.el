(use-package yaml-mode)
(use-package markdown-mode)
(use-package lua-mode)
(use-package erlang)
(use-package haskell-mode)
(use-package typescript-mode)
(use-package flutter)
(use-package swift-mode)
(use-package web-mode)
(use-package rustic
  :hook
  (rustic-mode . (lambda ()
              (add-hook 'before-save-hook 'eglot-format-buffer nil 'make-it-local)))
  :config
  (setq rustic-lsp-client 'eglot))
(use-package dart-mode
  :hook
  (dart-mode . (lambda ()
              (add-hook 'before-save-hook 'eglot-format-buffer nil 'make-it-local)))
  (dart-mode . (lambda ()
       (setq-default eglot-workspace-configuration
        '((:dart . (:completeFunctionCalls t  :applyEdit t)))))))
