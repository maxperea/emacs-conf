(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-set-leader '(normal visual) (kbd "<SPC>"))
  ;; Disable return and tab, while retaining jumping.
  (with-eval-after-load 'evil-maps (define-key evil-motion-state-map (kbd "RET") nil))
  (with-eval-after-load 'evil-maps (define-key evil-motion-state-map (kbd "TAB") nil))
  (define-key input-decode-map "\C-i" [C-i])
  (evil-define-key 'normal 'global
    (kbd "C-u") 'evil-scroll-up
    (kbd "<C-i>") 'evil-jump-forward)
  ;; TODO: Unbind "q" from evil-macro in temporary buffers.
  (evil-mode))

(use-package key-chord
  :config
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-mode 1))

(use-package evil-collection
  :after evil
  :config
  (with-eval-after-load 'magit
    (evil-collection-magit-setup))
  (with-eval-after-load 'dired
    (evil-collection-dired-setup)))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
