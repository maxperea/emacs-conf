(use-package evil
  :init
   (setq-default evil-want-keybinding nil
                 evil-want-C-i-jump nil)
  :config
  (evil-set-leader '(normal visual) (kbd "<SPC>"))
  ;; Disable return and tab, while retaining jumping.
  (with-eval-after-load 'evil-maps (define-key evil-motion-state-map (kbd "RET") nil))
  (with-eval-after-load 'evil-maps (define-key evil-motion-state-map (kbd "TAB") nil))
   (define-key input-decode-map "\C-i" [C-i]
              )

  (evil-define-key 'insert 'global
    (kbd "C-n") nil
    (kbd "C-p") nil)

  (evil-define-key 'normal 'global
    (kbd "q") nil
    (kbd "m") 'evil-record-macro
    (kbd "C-u") 'evil-scroll-up
    (kbd "C-n") nil
    (kbd "<C-i>") 'evil-jump-forward)
  ;; TODO: Unbind "q" from evil-macro in temporary buffers.
  (evil-mode))

(define-key evil-insert-state-map "j" #'cofi/maybe-exit)

(evil-define-command cofi/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "j")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?k)
               nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?k))
    (delete-char -1)
    (set-buffer-modified-p modified)
    (push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
                          (list evt))))))))

(use-package evil-collection
  :after evil
  :config
  (with-eval-after-load 'compile
    (evil-collection-compile-setup))
  (with-eval-after-load 'magit
    (evil-collection-magit-setup))
  (with-eval-after-load 'dired
    (evil-collection-dired-setup)))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :hook
  ((rustic-mode dart-mode) . (lambda ()
                           (push '(?< . ("<" . ">")) evil-surround-pairs-alist)))
  :config
  (global-evil-surround-mode 1))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
