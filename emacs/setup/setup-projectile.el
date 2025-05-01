;;; setup-projectile.el --- Project navigation and management with Ivy completion -*- lexical-binding: t; -*-

;; Copyright (C) 2025 BKR

;; Author: Beshr Kayali Reinholdsson <me@beshr.com>
;; URL: https://github.com/beshrkayali/system
;; Version: 2025.4
;; Package-Requires: ((emacs "30.1") (projectile "2.8.0") (counsel-projectile "0.3.0"))

;;; Commentary:

;; Focused project management configuration using Projectile with
;; Ivy integration for enhanced project navigation, discovery,
;; and management.  This module assumes setup-ivy.el is loaded.
;; Includes recursive project discovery and optimization for fast tools.

;;; Code:

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  ;; Use Ivy for completion (relies on setup-ivy.el)
  (projectile-completion-system 'ivy)

  ;; Performance optimizations
  (projectile-indexing-method 'alien)
  (projectile-enable-caching t)

  ;; Project detection - fixed to search recursively through subdirectories
  (projectile-project-search-path '(("~/src" . 3))) ; Search 3 levels deep
  (projectile-auto-discover t)

  ;; Behavior
  (projectile-switch-project-action #'projectile-dired)
  (projectile-require-project-root nil) ; use projectile even outside projects
  (projectile-current-project-on-switch 'keep) ; don't ask which project when there's only one
  (projectile-kill-buffers-filter 'kill-only-files) ; only kill file buffers when switching projects

  :config
  ;; Force Projectile to re-index your projects
  (projectile-discover-projects-in-search-path)

  ;; Clear projectile cache to force re-discovery
  (projectile-invalidate-cache nil)

  ;; Use the faster fd or ripgrep tools when available
  (when (executable-find "fd")
    (setq projectile-generic-command "fd . -0 --type f --color=never"
          projectile-git-command projectile-generic-command))
  (when (executable-find "rg")
    (setq projectile-generic-command "rg --files --follow --null"
          projectile-git-command projectile-generic-command))

  ;; Create missing directories when expanding files
  (setq projectile-create-missing-test-files t)

  ;; Recommended keymap prefix using Super-p (Option-p on Mac)
  (when (eq system-type 'darwin)
    (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))

  ;; Enable Projectile globally
  (projectile-mode +1)

  ;; Integrate with savehist for persistence
  (add-to-list 'savehist-additional-variables 'projectile-project-command-history)

  ;; Explicitly run project discovery
  (projectile-discover-projects-in-search-path))

;; Add counsel-projectile for Ivy integration
(use-package counsel-projectile
  :ensure t
  :after (counsel projectile)
  :config
  ;; This automatically replaces all projectile commands with counsel versions
  (counsel-projectile-mode 1))

;; Add magit integration for git projects
(use-package magit
  :ensure t
  :bind (:map projectile-command-map
              ("v" . magit-status)))

(message "Projectile config complete.")
(provide 'setup-projectile)
;;; setup-projectile.el ends here
