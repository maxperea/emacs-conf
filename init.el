;; This init file bootstraps use-package + straight if necessary,
;; before continuing initialization.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

(use-package exec-path-from-shell)
(exec-path-from-shell-initialize)

(setenv "LIBRARY_PATH"
    (string-join
     '("/opt/homebrew/opt/gcc/lib/gcc/13"
       "/opt/homebrew/opt/libgccjit/lib/gcc/13"
       "/opt/homebrew/opt/gcc/lib/gcc/13/gcc/aarch64-apple-darwin22/13")
     ":"))

;; Files to load
(setq config-base-directory "~/dev/emacs-conf/")

(setq my-init-file	(expand-file-name "init.el"	 config-base-directory))
(setq my-keys-file	(expand-file-name "keys.el"	 config-base-directory))
(setq my-modules-file	(expand-file-name "modules.el"	 config-base-directory))
(setq my-languages-file (expand-file-name "languages.el" config-base-directory))
(setq my-custom-file	(expand-file-name "custom.el"	 config-base-directory))
(setq my-evil-file	(expand-file-name "evil.el"	 config-base-directory))

(load my-modules-file)
(load my-languages-file)
(load my-custom-file)
(load my-evil-file)
(load my-keys-file)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
