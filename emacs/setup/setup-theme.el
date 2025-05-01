; Theme - modus-vivendi dark theme (built into Emacs 28+)

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
(defun my/toggle-modus-theme ()
  "Toggle between modus-vivendi and modus-operandi themes."
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-vivendi)
      (progn
        (disable-theme 'modus-vivendi)
        (load-theme 'modus-operandi t))
    (disable-theme 'modus-operandi)
    (load-theme 'modus-vivendi t)))

;; Bind theme toggle to a key
(global-set-key (kbd "C-c t") 'my/toggle-modus-theme)


; Mode line config

;; Adaptive minimal mode line configuration
(defun my/mode-line-adaptive-colors ()
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
(defun my/set-modeline-format ()
  "Set the mode line format with current theme-appropriate colors."
  (let* ((colors (my/mode-line-adaptive-colors))
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
(my/set-modeline-format)

;; Update mode line when theme changes
(defun my/update-modeline-on-theme-change (&rest _)
  "Update mode line when theme changes."
  (my/set-modeline-format))

;; Add hook to update colors when theme changes
(advice-add 'load-theme :after #'my/update-modeline-on-theme-change)

;; Remove some unnecessary minor modes from display
(setq-default mode-line-format-right-align nil)



; Font Configuration
(defun my/set-font ()
  "Set default font to Pragmasevka Nerd Font, size 16."
  (interactive)
  (when (member "Pragmasevka Nerd Font" (font-family-list))
    ;; Primary font
    (set-face-attribute 'default nil
                        :family "Pragmasevka Nerd Font"
                        :height 160
                        :weight 'normal)

    ;; Fixed-pitch face inherits from default
    (set-face-attribute 'fixed-pitch nil
                        :family "Pragmasevka Nerd Font"
                        :height 1.0)

    ;; Variable-pitch face - for prose text (optional)
    (set-face-attribute 'variable-pitch nil
                        :family "Pragmasevka Nerd Font"
                        :height 1.0)))


(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (my/set-font))))
  (my/set-font))

;; --

(message "Theme config complete.")
(provide 'setup-theme)