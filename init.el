;; This init file bootstraps use-package + straight if necessary,
;; before continuing initialization.

;; Configure file directory for config files.
(setq config-base-directory "~/Development/emacs-conf/")

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
(load (expand-file-name "evil.el"      config-base-directory))
(load (expand-file-name "modules.el"   config-base-directory))
(load (expand-file-name "languages.el" config-base-directory))
(load (expand-file-name "custom.el"    config-base-directory))
(load (expand-file-name "keys.el"      config-base-directory))

