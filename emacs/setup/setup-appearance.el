;;; setup-appearance.el --- Theme and mode-line customization -*- lexical-binding: t; -*-

;; Copyright (C) 2025 BKR

;; Author: Beshr Kayali Reinholdsson <me@beshr.com>
;; URL: https://github.com/beshrkayali/system
;; Version: 2025.4
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Provides a comprehensive theme setup with adaptive mode-line,
;; custom font configuration, and theme toggling capabilities.
;; Configures Modus themes with customized settings for both
;; dark and light variants.

;;; Code:

;; Theme - modus-vivendi dark theme (built into Emacs 28+)
(use-package emacs
  :config
  ;; Theme customization
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui nil
        modus-themes-custom-backgrounds nil
        modus-themes-prompts '(bold)
        modus-themes-completions '((selection . (semibold italic text-also))
                                   (popup . (accented)))
        modus-themes-org-blocks 'gray-background
        modus-themes-region '(bg-only no-extend))

  ;; Load the theme
  (load-theme 'modus-vivendi t))

;; Toggle function to switch between light/dark modus themes
(defun setup-appearance-toggle-modus ()
  "Toggle between modus-vivendi and modus-operandi themes."
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-vivendi)
      (progn
        (disable-theme 'modus-vivendi)
        (load-theme 'modus-operandi t))
    (disable-theme 'modus-operandi)
    (load-theme 'modus-vivendi t)))

;; Bind theme toggle to a key (using C-c T which is allowed for user configs)
(global-set-key (kbd "C-c C-t") 'setup-appearance-toggle-modus)

;; Mode line config
;; Adaptive minimal mode line configuration
(defun setup-appearance-mode-line-adaptive-colors ()
  "Return mode line colors appropriate for current theme."
  (let ((is-dark (eq (car custom-enabled-themes) 'modus-vivendi)))
    (if is-dark
        ;; Dark theme colors
        '(:mode-line-accent "#335577"
                            :mode-line-buffer "#223344"
                            :mode-line-position "#444466"
                            :mode-line-fg "white")
      ;; Light theme colors
      '(:mode-line-accent "#bbddff"
                          :mode-line-buffer "#d0e0f0"
                          :mode-line-position "#e0e0ff"
                          :mode-line-fg "black"))))

;; Minimal mode line with padding and adaptive colors
(defun setup-appearance-set-modeline-format ()
  "Set the mode line format with current theme-appropriate colors."
  (let* ((colors (setup-appearance-mode-line-adaptive-colors))
         (accent (plist-get colors :mode-line-accent))
         (buffer-bg (plist-get colors :mode-line-buffer))
         (position-bg (plist-get colors :mode-line-position))
         (fg (plist-get colors :mode-line-fg)))
    (setq-default mode-line-format
                  `("%e"
                    (:propertize " " display (raise 0.3))  ; Top padding
                    mode-line-front-space
                    (:propertize
                     (" " mode-line-mule-info mode-line-modified "  ")
                     face (:background ,accent :foreground ,fg))
                    (:propertize
                     ("  " mode-line-buffer-identification "  ")
                     face (:background ,buffer-bg :foreground ,fg))
                    (:propertize
                     ("  " mode-line-position "  ")
                     face (:background ,position-bg :foreground ,fg))
                    mode-line-modes
                    "  "
                    mode-line-misc-info
                    mode-line-end-spaces
                    (:propertize " " display (raise -0.3))))))  ; Bottom padding

;; Set initial mode line format
(setup-appearance-set-modeline-format)

;; Update mode line when theme changes
(defun setup-appearance-update-modeline-on-theme-change (&rest _)
  "Update mode line when theme changes."
  (setup-appearance-set-modeline-format))

;; Add hook to update colors when theme changes
(advice-add 'load-theme :after #'setup-appearance-update-modeline-on-theme-change)

;; Remove some unnecessary minor modes from display
(setq-default mode-line-format-right-align nil)

;; Font Configuration
(defun setup-appearance-set-font ()
  "Set default font to Pragmasevka Nerd Font, size 16."
  (interactive)
  (when (member "Pragmasevka Nerd Font" (font-family-list))
    ;; Primary font
    (set-face-attribute 'default nil
                        :family "Pragmasevka Nerd Font"
                        :height 140
                        :weight 'normal)

    ;; Fixed-pitch face inherits from default
    (set-face-attribute 'fixed-pitch nil
                        :family "Pragmasevka Nerd Font"
                        :height 1.0)

    ;; Variable-pitch face - for prose text (optional)
    (set-face-attribute 'variable-pitch nil
                        :family "Pragmasevka Nerd Font"
                        :height 1.0)))

;; Set font after frame creation to work with both daemon and normal mode
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (setup-appearance-set-font))))
  (setup-appearance-set-font))

(message "Theme config complete.")
(provide 'setup-appearance)
;;; setup-appearance.el ends here
