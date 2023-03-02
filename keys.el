;; Core
(general-def 'normal 'global
  ;; Eval
  "<leader>ee" 'eval-defun
  "<leader>eb" 'eval-buffer
  "<leader>SPC"'execute-extended-command
  "<leader>sh" 'eshell
  ;; Navigation
  "DEL"        'consult-buffer
  "<leader>fs" 'save-buffer
  "<leader>fS" 'save-some-buffers
  "<leader>ff" 'find-file
  "<leader>fo" 'consult-buffer-other-window
  "<leader>bp" 'consult-project-buffer
  "<leader>ll" 'consult-focus-lines
  "<leader>bo" 'consult-buffer-other-window
  "<leader>wq" 'evil-quit
  "<leader>ww" 'save-and-quit
  "<leader>qf" 'evil-quit-all
  "<leader>qr" 'delete-frame
  "<leader>wh" 'evil-window-left
  "<leader>wl" 'evil-window-right
  "<leader>wj" 'evil-window-down
  "<leader>wk" 'evil-window-up
  "<leader>wv" 'evil-window-vsplit
  "<leader>ws" 'evil-window-split
  "<leader>wm" 'delete-other-windows
  "<leader>h"  'evil-window-set-height
  "<leader>H"  'evil-window-set-width
  ;; Search
  "<leader>/"  'consult-ripgrep
  ","          'consult-line
  "C-,"        'consult-line-multi
  ;; Flymake
  "TAB"        'flymake-goto-next-error
  "<leader>aN" 'consult-flymake
  "<leader>aP" (lambda () (interactive) (consult-flymake t))
  ;; Paste
  "C-p"        'consult-yank-pop
  ;; Text
  "<leader>fp" 'fill-paragraph
  ;; Theme
  "<leader>t"  'consult-theme
  ;; Winner
  "L"          'winner-undo)

;; Project
(general-def 'normal 'global
  "C-<backspace>" 'project-find-file
  "<leader>pc"    'project-compile
  "<leader>pu"    'project-shell-command
  "<leader>ps"    'project-eshell
  "<leader>pk"    'project-kill-buffers
  "<leader>pr"    'project-query-replace-regexp
  "<leader>pp"    'my/project-switch-project)

  ;; Files
(general-def 'normal 'global
  "<leader>fi" (lambda () (interactive) (find-file (expand-file-name "init.el"    config-base-directory)))
  "<leader>fm" (lambda () (interactive) (find-file (expand-file-name "modules.el" config-base-directory)))
  "<leader>fk" (lambda () (interactive) (find-file (expand-file-name "keys.el"    config-base-directory)))
  "<leader>fc" (lambda () (interactive) (find-file (expand-file-name "custom.el"  config-base-directory)))
  "<leader>fl" (lambda () (interactive) (find-file (expand-file-name "languages.el"  config-base-directory)))
  "<leader>fn" (lambda () (interactive) (find-file org-default-notes-file))
  "<leader>fj" (lambda () (interactive) (find-file org-capture-journal-file))
  "<leader>ft" (lambda () (interactive) (find-file org-capture-todo-file)))

;; Org
(general-def 'normal 'org-mode-map
  "<RET>" 'org-open-at-point)

(general-def 'normal 'global
  "<leader>X" 'org-capture
  "<leader>S" 'consult-org-agenda
  "<leader>A" 'org-agenda
  "<leader>L" 'org-agenda-list
  "<leader>T" 'org-todo-list)

;; Dired
(general-def 'normal 'dired-mode-map
  "<SPC>" nil)

;; Eglot
(general-def 'normal 'prog-mode-map
  "gr"		'xref-find-references
  "K"		'eldoc-doc-buffer
  "RET"         'eglot-code-actions
  "<leader>ai"	'eglot-find-implementation
  "<leader>lr"	'eglot-rename
  "<leader>ae"	'flymake-show-project-diagnostics)

;; Flutter
(general-def 'normal 'global
  "<leader>r" 'flutter-run-or-hot-reload)

;; Magit
(general-def 'normal 'global
  "<leader>gg" 'magit)

;; Git-timemachine
;; (general-def 'normal 'global
;;   "<leader>gt" 'git-timemachine)

;; (general-def 'normal 'git-timemachine-mode
;;   "C-p" 'git-timemachine-show-previous-revision
;;   "C-n" 'git-timemachine-show-next-revision)

;; Undo tree
(general-def 'normal 'global
  "u" 'undo-tree-undo
  "C-r" 'undo-tree-redo)

;; Custom functions
(general-def 'normal 'global
  "<leader>ss" 'toggle-frame-maximized-undecorated
  "<leader>sd" 'toggle-frame-maximized-undecorated-desktop
  "<leader>n" 'mpd/show-file-name)


;; Avy
(general-def 'normal 'global
  "-" 'avy-goto-char-timer
  "C-; C-;" 'avy-goto-char
  "<leader> c l" 'avy-copy-line
  "<leader> m l" 'avy-move-line
  "<leader> k l" 'avy-kill-line
  "<leader> c r" 'avy-copy-region
  "<leader> m r" 'avy-move-region
  "<leader> k r" 'avy-kill-region)
