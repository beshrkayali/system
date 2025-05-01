;;; init.el --- Personal configuration for BKR -*- lexical-binding: t; -*-

;; Copyright (C) 2025 BKR

;; Author: Beshr Kayali Reinholdsson <me@beshr.com>
;; URL: https://github.com/beshrkayali/system
;; Version: 2025.4
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Main initialization file for my Emacs setup.
;; Loads modular configuration files from the setup directory.

;;; Code:

;; Load additional setup scripts
(add-to-list 'load-path (expand-file-name "setup" user-emacs-directory))

;; Store customizations in a separate file, for now ignore it
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (when (file-exists-p custom-file) (load custom-file))

;; Packages
(setq package-selected-packages
      '(use-package
         diminish ;; Diminished modes are minor modes with no modeline display
         ivy      ;; Completion frontend
         ivy-rich
         all-the-icons-ivy-rich
         ivy-posframe
         ivy-prescient
         flx
         projectile ;; For managing projects
         counsel-projectile

         ;; Modes
         ag
         company
         company-box
         nix-mode
         yasnippet
         yasnippet-snippets
         prog-mode
         which-key
         caddyfile-mode
         nim-mode
         markdown-mode
         yaml-mode
         rust-mode
         python-mode
         vue-mode
         lua-mode
         typescript-mode
         toml-mode
         jinja2-mode
         go-mode
         dockerfile-mode
         docker-compose-mode
         meson-mode
         olivetti
         lispy
         magit
         compile
         whitespace
         smartparens
         flycheck

         ;; Extra
         package-lint
         universal-emotions-emoticons))

;; Make this list definitive by preventing customization system from modifying it
(put 'package-selected-packages 'standard-value
     (list (custom-quote package-selected-packages)))

;; Setup things
(add-hook
 'after-init-hook
 (lambda ()
   (progn
     (require 'setup-base)
     (require 'setup-appearance)
     (require 'setup-ivy)
     (require 'setup-code)
     (require 'setup-projectile))))

(provide 'init)
;;; init.el ends here
