;; Load additional setup scripts
(add-to-list 'load-path (expand-file-name "setup" user-emacs-directory))

;; Store customizations in a separate file, for now ignore it
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
; (when (file-exists-p custom-file) (load custom-file))

;; Packages
(setq package-selected-packages 
      '(use-package
        diminish              ;; Diminished modes are minor modes with no modeline display
        ivy                   ;; Completion frontend
        ivy-rich
        all-the-icons-ivy-rich
        ivy-posframe
        ivy-prescient
        flx
        projectile            ;; For managing projects
        counsel-projectile

        ;; Modes
        caddyfile-mode
        nim-mode
        markdown-mode
        yaml-mode
        python-mode
        vue-mode
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

        ;; Extra
        universal-emotions-emoticons
     ))

;; Make this list definitive by preventing customization system from modifying it
(put 'package-selected-packages 'standard-value 
     (list (custom-quote package-selected-packages)))

;; Setup things
(add-hook
 'after-init-hook
 (lambda ()
   (progn
     (require 'setup-emacs)
     (require 'setup-theme)
     (require 'setup-ivy)
     (require 'setup-projectile)
     )))

