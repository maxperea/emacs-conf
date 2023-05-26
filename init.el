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

;; Files to load
(setq config-base-directory "~/Development/emacs-conf/")

(setq my-init-file	(expand-file-name "init.el"	 config-base-directory))
(setq my-modules-file	(expand-file-name "modules.el"	 config-base-directory))
(setq my-languages-file (expand-file-name "languages.el" config-base-directory))
(setq my-evil-file	(expand-file-name "evil.el"	 config-base-directory))
(setq my-custom-file	(expand-file-name "custom.el"	 config-base-directory))
(setq my-keys-file	(expand-file-name "keys.el"	 config-base-directory))

(load my-modules-file)
(load my-languages-file)
(load my-custom-file)
(load my-evil-file)
(load my-keys-file)
