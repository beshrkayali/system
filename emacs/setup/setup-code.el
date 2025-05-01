;;; setup-code.el --- Programming modes configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 BKR

;; Author: Beshr Kayali Reinholdsson <me@beshr.com>
;; URL: https://github.com/beshrkayali/system
;; Version: 2025.4
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Configuration for various programming language modes with sensible defaults.
;; Includes setup for multiple languages and development tools.

;;; Code:

;; Common development settings
(use-package prog-mode
  :ensure nil ; built-in
  :hook ((prog-mode . display-line-numbers-mode)
         (prog-mode . show-paren-mode)
         (prog-mode . electric-pair-mode)
         (prog-mode . subword-mode)) ; Better navigation in camelCase
  :config
  ;; Highlight current line in programming modes
  (add-hook 'prog-mode-hook #'hl-line-mode)

  ;; Comment configuration
  (setq comment-auto-fill-only-comments t))

;; Improve code navigation
(use-package which-key
  :ensure t
  :diminish
  :custom
  (which-key-idle-delay 0.5)
  :config
  (which-key-mode 1))

;; Magit - Git integration
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  (magit-refs-show-commit-count 'all)
  (magit-section-initial-visibility-alist '((stashes . hide))))

;; Lispy for structural editing of Lisp code
(use-package lispy
  :ensure t
  :hook ((emacs-lisp-mode . lispy-mode)
         (lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode))
  :config
  ;; Make lispy less aggressive
  (setq lispy-close-quotes-at-end-p nil))

;; Olivetti for distraction-free writing
(use-package olivetti
  :ensure t
  :bind (("C-c o" . olivetti-mode))
  :custom
  (olivetti-body-width 100)
  (olivetti-minimum-body-width 80)
  :hook ((text-mode . (lambda () (olivetti-mode 1)))))

;; Python mode
(use-package python-mode
  :ensure t
  :mode "\\.py\\'"
  :interpreter "python"
  :custom
  (python-indent-offset 4)
  (python-shell-interpreter "python3")
  (python-shell-completion-native-enable nil)
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (setq tab-width 4)
              (setq fill-column 88) ; Black formatter uses 88
              (setq-local electric-indent-chars (delq ?: electric-indent-chars)))))

;; Rust mode
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :custom
  (rust-format-on-save t)
  (rust-indent-offset 4))

;; Go mode
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook ((go-mode . (lambda ()
                      (setq tab-width 4)
                      (setq indent-tabs-mode t)))))

;; Typescript mode
(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" "\\.tsx\\'")
  :custom
  (typescript-indent-level 2))

;; Vue mode
(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'"
  :custom
  (mmm-submode-decoration-level 0) ; Less aggressive highlighting
  :config
  (setq-default js-indent-level 2))

;; YAML mode
(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" "\\.yml\\'"))

;; Markdown mode
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "pandoc")
  (markdown-fontify-code-blocks-natively t))

;; TOML mode
(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")

;; Nim mode
(use-package nim-mode
  :ensure t
  :mode "\\.nim\\'")

;; Jinja2 mode
(use-package jinja2-mode
  :ensure t
  :mode (("\\.j2\\'" . jinja2-mode)
         ("\\.jinja2\\'" . jinja2-mode)))

;; Docker related modes
(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'" "\\.dockerfile\\'"))

(use-package docker-compose-mode
  :ensure t
  :mode ("docker-compose\\.yml\\'" "docker-compose\\.yaml\\'"))

;; Caddyfile mode
(use-package caddyfile-mode
  :ensure t
  :mode ("Caddyfile\\'" "\\.caddy\\'"))

;; Meson build system
(use-package meson-mode
  :ensure t
  :mode ("meson\\.build\\'" "meson_options\\.txt\\'"))

;; Enhance compilation mode
(use-package compile
  :ensure nil ; built-in
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-always-kill t)
  :config
  ;; Auto-close compilation window if successful
  (setq compilation-finish-functions
        (lambda (_buf status)
          (when (and (string-match-p "\\*compilation\\*" (buffer-name _buf))
                     (string-match-p "finished" status))
            (with-current-buffer _buf
              (let ((w (get-buffer-window _buf)))
                (when w (with-selected-window w (delete-window)))))))))

;; Whitespace mode configuration
(use-package whitespace
  :ensure nil ; built-in
  :diminish
  :hook ((prog-mode . whitespace-mode))
  :custom
  (whitespace-line-column 100)
  (whitespace-style '(face trailing lines-tail)))

;; Smart parentheses
(use-package smartparens
  :ensure t
  :diminish
  :hook ((prog-mode . smartparens-mode))
  :config
  (require 'smartparens-config))

;; Setup flycheck for syntax checking
(use-package flycheck
  :ensure t
  :diminish
  :hook ((prog-mode . flycheck-mode))
  :custom
  (flycheck-indication-mode 'right-fringe)
  (flycheck-check-syntax-automatically '(save mode-enabled)))

(message "Code setup complete.")
(provide 'setup-code)
;;; setup-code.el ends here
