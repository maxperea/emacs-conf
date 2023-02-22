;; A space for your own functions.

(defun my/project-switch-project (dir)
  "Custom version of 'project-switch-project' that does
not ask for action and immediately provides a list of
project files."
  (interactive (list (project-prompt-project-dir)))
    (let ((project-current-directory-override dir))
      (call-interactively 'project-find-file)))
