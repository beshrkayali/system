; Cnfiguration for Ivy, Counsel, and Swiper with some customizations


;; Core Ivy setup
(use-package ivy
  :ensure t
  :diminish
  :custom
  (ivy-use-virtual-buffers t)               ; Add recent files and bookmarks to buffers list
  (ivy-count-format "%d/%d ")               ; Show index and count in the prompt
  (ivy-height 15)                           ; Taller minibuffer to show more candidates
  (ivy-wrap t)                              ; Wrap around candidates
  (ivy-fixed-height-minibuffer t)           ; Fixed height for aesthetics
  (ivy-on-del-error-function #'ignore)      ; Don't quit minibuffer on DEL at beginning
  (ivy-initial-inputs-alist nil)            ; Remove initial ^ input
  (ivy-use-selectable-prompt t)             ; Make the prompt line selectable
  (ivy-rich-parse-remote-buffer nil)        ; Better performance with remote files

  :bind (("C-s" . swiper)                   ; Better C-s search
         ("C-r" . swiper-backward)          ; Better C-r search
         ("C-x b" . ivy-switch-buffer)      ; Better buffer switching
         ("C-c v" . ivy-push-view)          ; Save window configurations
         ("C-c V" . ivy-pop-view)           ; Restore window configurations
         :map ivy-minibuffer-map
         ("C-r" . ivy-previous-line-or-history) ; Search history or navigate up
         ("M-v" . yank)                      ; Paste in minibuffer
         ("C-l" . ivy-alt-done)              ; Complete directory or action
         :map ivy-switch-buffer-map
         ("C-k" . ivy-switch-buffer-kill))   ; Kill buffer from list

  :config
  (ivy-mode 1)

  ;; Save minibuffer history
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'ivy-history)
  (add-to-list 'savehist-additional-variables 'ivy-views))

;; Enhanced commands with Counsel
(use-package counsel
  :ensure t
  :after ivy
  :diminish
  :custom
  (counsel-yank-pop-separator "\n────────\n") ; Add separator between kill ring items
  (counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")
  (counsel-describe-function-function #'helpful-callable) ; Better help buffers (if helpful is installed)
  (counsel-describe-variable-function #'helpful-variable)

  :bind (("M-x" . counsel-M-x)              ; Enhanced M-x
         ("C-x C-f" . counsel-find-file)    ; Enhanced find file
         ("C-x C-r" . counsel-recentf)      ; Browse recent files
         ("C-x C-b" . counsel-ibuffer)      ; Enhanced ibuffer
         ("C-c g" . counsel-git)            ; Find file in git repo
         ("C-c j" . counsel-git-grep)       ; Grep in git repo
         ("C-c L" . counsel-locate)         ; Locate files
         ("C-c i" . counsel-imenu)          ; Navigate by semantic units
         ("C-h v" . counsel-describe-variable)
         ("C-h f" . counsel-describe-function)
         ("C-h o" . counsel-describe-symbol)
         ("C-c u" . counsel-unicode-char)   ; Insert unicode character
         ("C-c s" . counsel-rg)             ; Fast file content search
         ("C-c d" . counsel-descbinds)      ; Show key bindings
         ("M-y" . counsel-yank-pop)         ; Enhanced clipboard history
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history)) ; History in minibuffer

  :config
  (counsel-mode 1)

  ;; Use faster search tools when available
  (when (executable-find "rg")
    (setq counsel-grep-base-command
          "rg -i -M 120 --no-heading --line-number --color never %s %s"))

  ;; Settings for counsel-find-file
  (setq counsel-find-file-at-point t))

;; Improved search with Swiper
(use-package swiper
  :ensure t
  :after ivy
  :custom
  (swiper-action-recenter t)       ; Recenter after navigation
  (swiper-include-line-number-in-search t)) ; Match line numbers

;; Enhance display in the minibuffer
(use-package ivy-rich
  :ensure t
  :after (ivy counsel)
  :custom
  (ivy-rich-path-style 'abbrev)     ; Abbreviate paths
  :config
  (ivy-rich-mode 1)
  ;; Better formatting for buffers
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; Add prescient for sorting by frequency
(use-package ivy-prescient
  :ensure t
  :after ivy
  :custom
  (ivy-prescient-retain-classic-highlighting t)
  (ivy-prescient-sort-commands
   '(:not swiper ivy-switch-buffer counsel-switch-buffer))
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1))

;; Enable fuzzy matching
(use-package flx
  :ensure t
  :after ivy
  :custom
  (ivy-re-builders-alist
   '((swiper . ivy--regex-plus)
     (counsel-rg . ivy--regex-plus)
     (t . ivy--regex-fuzzy))))

;; Add helpful Icons
(use-package all-the-icons-ivy-rich
  :ensure t
  :after (ivy all-the-icons)
  :init
  (all-the-icons-ivy-rich-mode 1))

;; Make Ivy appear in a posframe (floating window)
(use-package ivy-posframe
  :ensure t
  :after ivy
  :custom
  (ivy-posframe-display-functions-alist
   '((swiper . ivy-posframe-display-at-point)
     (complete-symbol . ivy-posframe-display-at-point)
     (counsel-M-x . ivy-display-function-fallback)
     (t . ivy-posframe-display-at-frame-center)))
  :config
  (ivy-posframe-mode 1)
  )

;; Mac-specific settings
(when (eq system-type 'darwin)
  (global-set-key (kbd "s-f") 'swiper))  ; CMD+f searches with Swiper on Mac

(message "Ivy configuration loaded.")
(provide 'setup-ivy)
;;; setup-ivy.el ends here
