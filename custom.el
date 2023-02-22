;; A space for your own functions.

(defun custom/project-switch-project (dir)
  "Custom version of 'project-switch-project' that does
not ask for action and immediately provides a list of
project files."
  (interactive (list (project-prompt-project-dir)))
    (with-temp-buffer
      (let ((default-directory dir)
            (project-current-inhibit-prompt t))
        (call-interactively 'project-find-file))))
