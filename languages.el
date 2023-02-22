(use-package yaml-mode)
(use-package markdown-mode)
(use-package lua-mode)
(use-package erlang)
(use-package haskell-mode)
(use-package typescript-mode)
(use-package flutter)
(use-package dart-mode
  :hook ((dart-mode . (lambda ()
	      (setq-default eglot-workspace-configuration
			    '((:dart . (:completeFunctionCalls t))))
	      (eglot-ensure)))))
(use-package rustic
  :config
  (setq rustic-lsp-client 'eglot)
  :hook
  ((rust-mode . eglot-ensure)))
