(use-package yaml-mode)
(use-package markdown-mode)
(use-package lua-mode)
(use-package erlang)
(use-package haskell-mode)
(use-package typescript-mode)
(use-package flutter)
(use-package swift-mode)

(use-package dart-mode
  :init
  (add-hook 'dart-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'eglot-format-buffer nil 'make-it-local)))
  :hook
  ((dart-mode . (lambda ()
       (setq-default eglot-workspace-configuration
        '((:dart . (:completeFunctionCalls t  :applyEdit t))))))))

(use-package rustic
  :config
  (setq rustic-lsp-client 'eglot))
