;;; angelscript-mode.el --- Major mode for editing Angelscript files

;; Define the list of keywords.
(defvar mylang-font-lock-keywords
  `(


    ("\\<\\(bool\\|void\\|int\\|float\\|char\\)\\>" . font-lock-builtin-face)
    ("\\<\\(true\\|false\\)\\>" . font-lock-builtin-face)

    ("\\<\\(delegate\\|const\\|enum\\|if\\|auto\\|else\\|class\\|while\\|for\\|return\\|this\\)\\>" . font-lock-keyword-face)
    ("\\<\\(switch\\|case\\|break\\)\\>" . font-lock-keyword-face)

    ("\\<\\(UCLASS\\|UFUNCTION\\|UPROPERTY\\)\\>" . font-lock-constant-face)

    ("\\<\\(class\\|enum\\)\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)" 2 font-lock-type-face)

    ;; Template types
    ("\\<\\([A-Za-z_][A-Za-z0-9_]*\\)<\\([A-Za-z_][A-Za-z0-9_]*\\)>\\s-+[A-Za-z_][A-Za-z0-9_]*" (1 font-lock-type-face) (2 font-lock-type-face))

    ;; Template functions
    ("\\<\\([A-Za-z_][A-Za-z0-9_]*\\)<[A-Za-z_][A-Za-z0-9_]*>" 1 font-lock-function-name-face)

    ;; Template function argument
    ("\\<\\([A-Za-z_][A-Za-z0-9_]*\\)<\\([A-Za-z_][A-Za-z0-9_]*\\)>" 2 font-lock-type-face)


     ;; Match floating-point numbers
    ("\\<\\([0-9]+\\.?[0-9]*\\|[0-9]*\\.[0-9]+\\)\\>" . font-lock-builtin-face)

    ;; Match numbers (integers)
    ("\\<[0-9]+\\>" . font-lock-builtin-face)

    ;; Match types (First of space separated words)
    ("\\<\\([A-Z][A-Za-z0-9_]*\\)\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)\\>" 1 font-lock-type-face)

    ;; Function names
    ("\\<\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*(" 1 font-lock-function-name-face)

    ;; Variables
    ("\\<\\([A-Za-z_][A-Za-z0-9_]*\\)\\>" . font-lock-variable-name-face)

    ;; Operators and such
    ("[<>!/+\\*&|=%-]" . font-lock-keyword-face)

    ;; C-style comments
    ("/\\*.*?\\*/" . font-lock-comment-face)

    ;; Single-line comments
    ("//.*$" . font-lock-comment-face)


    ("\\(\".*?\"\\)" . font-lock-string-face)
    ("\\('\\w'\\)" . font-lock-constant-face)

    ))

;; Define the mode itself
(define-derived-mode angelscript-mode prog-mode "Angelscript"
  "Major mode for editing Angelscript (.as) files."
  ;; Code for syntax highlighting
  (setq font-lock-defaults '((mylang-font-lock-keywords)))
  ;; Set up the syntax table for comments
  (let ((st (make-syntax-table)))
    ;; Add C-style comment syntax ('/* ... */')
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    ;; Newline ends a C-style comment
    (modify-syntax-entry ?\n "> b" st)
    ;; Set this as the current syntax table
    (set-syntax-table st))
  )

;; Add the mode to the `features' list
(provide 'angelscript-mode)

;;; angelscript-mode.el ends here
