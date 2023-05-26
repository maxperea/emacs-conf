;; Small, custom functions.
(defun my/project-switch-project (dir)
  "\"Switch\" to another project by running an Emacs command.
The available commands are presented as a dispatch menu
made from `project-switch-commands'.

When called in a program, it will use the project corresponding
to directory DIR."
  (interactive (list (project-prompt-project-dir)))
  (let ((command 'project-find-file))
    (let ((project-current-directory-override dir))
      (call-interactively command))))

(defun mpd/load-aoc-input (day)
  "Loads latest aoc."
  (interactive "sEnter day:")
  (shell-command
   (concat "get_aoc_day_2022 " day)))

(defun mpd/load-aoc-instructions (day)
  "Loads latest aoc."
  (interactive "sEnter day:")
  (shell-command
   (concat "get_aoc_day_2022 " day)))

(defun mpd/set-font-size (size)
  "Sets font size for new frames."
  (interactive "sFont size:")
  (set-frame-font (concat "JetBrains Mono " size)))

(defun mpd/show-file-name () (interactive) (message (buffer-file-name)))

(defun toggle-frame-maximized-undecorated ()
  (interactive)
  (let* ((frame (selected-frame))
	 (on? (and (frame-parameter frame 'undecorated)
		   (eq (frame-parameter frame 'fullscreen) 'maximized)))
	 (geom (frame-monitor-attribute 'geometry))
	 (x (nth 0 geom))
	 (y (nth 1 geom))
	 (display-height (nth 3 geom))
	 (display-width (nth 2 geom))
	 (cut (if on?
		  (if ns-auto-hide-menu-bar 26 50)
		(if ns-auto-hide-menu-bar 4 26))))
    (set-frame-position frame x y)
    (set-frame-parameter frame 'fullscreen-restore 'maximized)
    (set-frame-parameter nil 'fullscreen 'maximized)
    (set-frame-parameter frame 'undecorated (not on?))
    (set-frame-height frame (- display-height (+ cut 16)) nil t)
    (set-frame-width frame (- display-width 20) nil t)
    (set-frame-position frame x y)))

(defun toggle-frame-maximized-undecorated-desktop ()
  (interactive)
  (let* ((frame (selected-frame))
	 (on? (and (frame-parameter frame 'undecorated)
		   (eq (frame-parameter frame 'fullscreen) 'maximized)))
	 (geom (frame-monitor-attribute 'geometry))
	 (x (nth 0 geom))
	 (y (nth 1 geom))
	 (display-height (nth 3 geom))
	 (display-width (nth 2 geom))
	 (cut (if on?
		  (if ns-auto-hide-menu-bar 26 50)
		(if ns-auto-hide-menu-bar 4 26))))
    (set-frame-position frame x y)
    (set-frame-parameter frame 'fullscreen-restore 'maximized)
    (set-frame-parameter nil 'fullscreen 'maximized)
    (set-frame-parameter frame 'undecorated (not on?))
    (set-frame-height frame display-height nil t)
    (set-frame-width frame (- display-width 20) nil t)
    (set-frame-position frame x y)))

;; TABA
(setq taba-server-dir "~/Development/sliding-server")
(setq taba-flutter-dir "~/Development/sliding-flutter")

(defun taba-build-all ()
  (interactive)
  (taba-build-ios)
  (taba-build-appbundle))

(defun taba-android-release ()
  "Builds apk and moves it to shared folder."
  (interactive)
  (compilation-start
   (concat "cd " taba-flutter-dir " && flutter build apk && cp build/app/outputs/flutter-apk/app-release.apk ~/phone_file_share/taba.apk")))

(defun taba-build-appbundle ()
  "Builds aab and moves it to shared folder."
  (interactive)
  (compilation-start
   (concat "cd " taba-flutter-dir " && flutter build appbundle && cp build/app/outputs/bundle/release/app-release.aab ~/phone_file_share/taba.aab")))

(defun taba-build-ios ()
  "Builds ios project."
  (interactive)
  (compilation-start
   (concat "cd " taba-flutter-dir " && flutter build ios")))

(defun taba-flutter-format-all ()
  (interactive)
  (compilation-start
   (concat "cd " taba-flutter-dir " && flutter format lib")))

(defun taba-build-and-run-local-docker ()
  (interactive)
  (compilation-start
   (concat "cd " taba-server-dir " && make docker-local-down && make docker-local-clean-up && make docker-local-build && make docker-local-up")))

;; Frame sizing
(defun mpd/frame/half-laptop ()
  (interactive)
  (set-frame-width nil 98)
  (set-frame-height nil 56))

(defun mpd/frame/full-laptop () (interactive)
       (set-frame-width nil 198)
       (set-frame-height nil 56))

;; Sliding Server
(defun taba-server-make-ct ()
  "Runs CT for the taba server. Required a server to be running."
  (interactive)
  (compilation-start
   (concat "cd " taba-server-dir " && make ct")))
