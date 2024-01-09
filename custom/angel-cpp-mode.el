;;; angelscript-mode.el --- Major mode for editing Angelscript files

;; Define the list of keywords.
;; (defvar mylang-font-lock-keywords
;;   `(("\\<\\(delegate\\|const\\|enum\\|if\\|auto\\|else\\|class\\|while\\|for\\|return\\)\\>" . font-lock-keyword-face)
;;     ("\\<\\(bool\\|void\\|int\\|float\\|char\\)\\>" . font-lock-type-face)
;;     ("\\<\\(true\\|false\\)\\>" . font-lock-constant-face)

;;      ;; Match floating-point numbers
;;     ("\\<\\([0-9]+\\.?[0-9]*\\|[0-9]*\\.[0-9]+\\)\\>" . font-lock-constant-face)
;;     ;; Match numbers (integers)
;;     ("\\<[0-9]+\\>" . font-lock-constant-face)

;;     ;; Match types (First of space separated words)
;;     ("\\<\\([A-Z][A-Za-z0-9_]*\\)\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)\\>" 1 font-lock-type-face)

;;     ("\\<\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*(" . font-lock-function-name-face)
;;     ("\\<\\([A-Za-z_][A-Za-z0-9_]*\\)\\>" . font-lock-variable-name-face)
;;     ("\\(\".*?\"\\)" . font-lock-string-face)
;;     ("\\('\\w'\\)" . font-lock-constant-face)
;;     ;; Operators and such
;;     ("[<>!\\/+\\-*:&|=%]" . font-lock-builtin-face)
;;     ;; C-style comments
;;     ("/\\*.*?\\*/" . font-lock-comment-face)
;;     ;; Single-line comments
;;     ("//.*$" . font-lock-comment-face)
;;     ))

;; Define the mode itself
(define-derived-mode angel-cpp-mode c++-mode "Angelscript CPP Mode"
  "Major mode for editing Angelscript (.as) files."
  ;; Code for syntax highlighting
  ;; (setq font-lock-defaults '((mylang-font-lock-keywords)))
    ;; Set up the syntax table for comments
  ;; Set up the syntax table for comments
  ;; (let ((st (make-syntax-table)))
  ;;   ;; Add C-style comment syntax ('/* ... */')
  ;;   (modify-syntax-entry ?/ ". 124b" st)
  ;;   (modify-syntax-entry ?* ". 23" st)
  ;;   ;; Newline ends a C-style comment
  ;;   (modify-syntax-entry ?\n "> b" st)
  ;;   ;; Set this as the current syntax table
  ;;   (set-syntax-table st))
  ;; Additional settings can be added here.
  )

;; Add the mode to the `features' list
(provide 'angel-cpp-mode)

;;; angelscript-mode.el ends here
