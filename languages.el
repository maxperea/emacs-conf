(use-package yaml-mode)
(use-package markdown-mode)
(use-package lua-mode)
(use-package erlang)
(use-package haskell-mode)
(use-package typescript-mode)
(use-package flutter)
(use-package swift-mode)
(use-package dart-mode
  :hook (;; (before-save . eglot-format-buffer)
	 (dart-mode . (lambda ()
	      (setq-default eglot-workspace-configuration
			    '((:dart . (:completeFunctionCalls t)))))))
  )
(use-package rustic
  :config
  (setq rustic-lsp-client 'eglot))
