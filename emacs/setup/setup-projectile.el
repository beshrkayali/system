;;; Projectile configuration (assumes setup-ivy.el is loaded)

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
  (add-to-list 'savehist-additional-variables 'projectile-project-command-history))

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

;; Explicitly run project discovery after config is loaded
(with-eval-after-load 'projectile
  (projectile-discover-projects-in-search-path))

;; --

(message "Projectile config complete.")
(provide 'setup-projectile)
