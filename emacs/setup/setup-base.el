;;; setup-base.el --- Base configuration for my setup -*- lexical-binding: t; -*-

;; Copyright (C) 2025 BKR

;; Author: Beshr Kayali Reinholdsson <me@beshr.com>
;; URL: https://github.com/beshrkayali/system
;; Version: 2025.4
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; This file contains base configuration for Emacs, setting up package
;; repositories and sensible defaults for a cleaner, more efficient
;; editing experience.

;;; Code:

;; Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Opinionated Defaults
(setenv "LANG" "en_US.UTF-8")                    ; Ensure consistent encoding
(setq inhibit-startup-message t)                 ; No splash screen, get straight to work
(set-default 'truncate-lines 1)                  ; Prefer horizontal scrolling over line wrapping
(defvar warning-minimum-log-level)
(setq warning-minimum-log-level :emergency)      ; Only show the most critical warnings
(setq warning-suppress-log-types '((comp)))      ; Silence compiler warnings
(setq use-package-verbose nil)                   ; Suppress package loading messages
(setq warning-minimum-level :error)              ; Only show errors, not warnings
(show-paren-mode 1)                              ; Highlight matching parentheses - essential!
(tool-bar-mode -1)                               ; Remove toolbar for more screen space
(menu-bar-mode -1)                               ; Remove menu bar for cleaner look
(setq inhibit-compacting-font-caches t)          ; Speed up rendering with large fonts
(setq auto-window-vscroll nil)                   ; Improves scrolling performance
(global-so-long-mode 1)                          ; Handle long lines gracefully
(global-display-line-numbers-mode)               ; Show line numbers globally
(column-number-mode t)                           ; Show column number in modeline
(global-hl-line-mode 1)                          ; Highlight current line
(setq ring-bell-function 'ignore)                ; Disable bell
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)              ; Use spaces instead of tabs
(delete-selection-mode 1)                        ; Replace selection when typing
(global-auto-revert-mode 1)                      ; Auto-refresh buffers when files change
(save-place-mode 1)                              ; Remember cursor position in files
(setq create-lockfiles nil)                      ; No need for lockfiles
(setq gc-cons-threshold 100000000)               ; 100MB - increase GC threshold
(setq read-process-output-max (* 1024 1024))     ; 1MB - smoother LSP experience

;; Diminish some modes
(when (require 'diminish nil 'noerror)
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode))


;; Zoom in/out on windows
(use-package zoom-window
  :ensure t
  :diminish
  :bind (("s-<return>" . 'zoom-window-zoom)):custom
  (zoom-window-mode-line-color "#6b7ec3") ; Blue-ish, works on white/dark theme variations
  )


;; Set default frame size and center position
(setq default-frame-alist '((width . 190)  (height . 70)))

;; Position the initial frame in the center
(defun setup-base-center-frame ()
  "Center the frame on screen."
  (interactive)
  (let* ((frame-w (frame-pixel-width))
         (frame-h (frame-pixel-height))
         (screen-w (display-pixel-width))
         (screen-h (display-pixel-height))
         (pos-x (/ (- screen-w frame-w) 2))
         (pos-y (/ (- screen-h frame-h) 2)))
    (set-frame-position (selected-frame) pos-x pos-y)))

;; Apply frame settings after initialization
(add-hook 'window-setup-hook 'setup-base-center-frame)

;; For new frames in daemon mode
(when (daemonp)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (run-with-idle-timer 0.1 nil 'setup-base-center-frame)))))

;; System specific
;; - Mac
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta) ; Use command as Meta - feels more natural on Mac
  (setq mac-option-modifier 'super) ; Use option as Super
  (setq mac-right-option-modifier 'none) ; Right option for special characters
  (add-to-list 'default-frame-alist
               '(ns-transparent-titlebar . t)) ; Clean titlebar
  (add-to-list 'default-frame-alist
               '(ns-appearance . dark)))         ; Match system appearance

(message "Base emacs config complete.")
(provide 'setup-base)
;;; setup-base.el ends here
