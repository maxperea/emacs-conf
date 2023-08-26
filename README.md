# Ergonomic Emacs

Custom emacs configuration with a focus on ergonomics.

## Setup

1. Link the init file.

```bash
ln -s /path/to/repo/init.el ~/.emacs

```

2. Change this variable to repo location.

```bash
;; Files to load
(setq config-base-directory "/path/to/repo/")

```

3. Launch emacs

## Files

### init.el

Bootstraps use-package, sets base directory variable and loads remaining files.

### modules.el

Contains most of the different packages, along with their configurations.

### languages.el

Contains language-specific packages.

### evil.el

Contains evil and related packages.

### keys.el

Contains keybindings.

### custom.el

Contains custom functions.
