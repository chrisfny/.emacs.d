(dolist (mode
         '(tool-bar-mode                ; No toolbars, more room for text
           scroll-bar-mode              ; No scroll bars either
           blink-cursor-mode
		   menu-bar-mode))          ; The blinking cursor gets old
  (funcall mode 0))

(dolist (mode	 
         '(abbrev-mode                  ; E.g. sopl -> System.out.println
           column-number-mode           ; Show column number in mode line
           delete-selection-mode        ; Replace selected text
           dirtrack-mode                ; directory tracking in *shell*
           global-so-long-mode          ; Mitigate performance for long lines
           recentf-mode                 ; Recently opened files
           savehist-mode                ; Prioritize recently used commands
		   electric-pair-mode           ; Adds closing bracksts
		   display-line-numbers-mode    ; Displays line numbers
           show-paren-mode))            ; Highlight matching parentheses
  (funcall mode 1))

(setq auto-revert-interval 1            ; Refresh buffers fast
      default-input-method "TeX"        ; Use TeX when toggling input method
      echo-keystrokes 0.1               ; Show keystrokes asap
      enable-recursive-minibuffers t    ; Allow recursive minibuffers
      frame-inhibit-implied-resize 1    ; Don't resize frame implicitly
      inhibit-startup-screen t          ; No splash screen please
      initial-scratch-message nil       ; Clean scratch buffer
      recentf-max-saved-items 10000     ; Show more recent files
      ring-bell-function 'ignore        ; Quiet
      scroll-margin 1                   ; Space between cursor and top/bottom
      sentence-end-double-space nil     ; No double space
      custom-file                       ; Customizations in a separate file
      (concat user-emacs-directory "custom.el"))

(setq-default tab-width 4                       ; Smaller tabs
              fill-column 79                    ; Maximum line width
               truncate-lines t                  ; Don't fold lines
               indent-tabs-mode t              ; Use spaces instead of tabs
               split-width-threshold 160         ; Split verticly by default
               split-height-threshold nil        ; Split verticly by default
               frame-resize-pixelwise t          ; Fine-grained frame resize
               auto-fill-function 'do-auto-fill) ; Auto-fill-mode everywhere

(set-language-environment    "UTF-8")
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)

(defvar emacs-autosave-directory
  (concat user-emacs-directory "autosaves/")
  "This variable dictates where to put auto saves. It is set to a
  directory called autosaves located wherever your .emacs.d/ is
  located.")

;; Sets all files to be backed up and auto saved in a single directory.
(setq backup-directory-alist
      `((".*" . ,emacs-autosave-directory))
      auto-save-file-name-transforms
      `((".*" ,emacs-autosave-directory t)))

(when (member "JetBrainsMono NFM" (font-family-list))
  (set-face-attribute 'default nil :font "JetBrainsMono NFM" :height 115))

(require 'use-package)
(setq use-package-always-ensure t)

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")
        ("nongnu"       . "https://elpa.nongnu.org/nongnu/"))
      package-archive-priorities
      '(("GNU ELPA"     . 15)
        ("MELPA"        . 10)
        ("MELPA Stable" . 5)
        ("nongnu"       . 0)))

(use-package catppuccin-theme
  :ensure t
  :config
  (load-theme 'catppuccin :no-confirm)
  (setq catppuccin-flavor 'mocha) ;; 'latte, 'frappe, 'macchiato, or 'mocha)
  (catppuccin-reload))

(defun soph/take-me-home ()
  (interactive)
  (if (looking-back "/" nil)
      (progn (call-interactively 'delete-minibuffer-contents) (insert "~/"))
    (call-interactively 'self-insert-command)))

(use-package vertico
  :bind (:map vertico-map ("~" . soph/take-me-home))
  :config
  (vertico-mode)
  (setq vertico-count 25))
(use-package vertico-posframe
  :init
  (setq vertico-posframe-parameters   '((left-fringe  . 12)    ;; Fringes
                                        (right-fringe . 12)
                                        (undecorated  . nil))) ;; Rounded frame
  :config
  (vertico-posframe-mode 1)
  (setq vertico-posframe-width        88                       ;; Narrow frame
        vertico-posframe-height       vertico-count            ;; Default height
        ;; Don't create posframe for these commands
        vertico-multiform-commands    '((consult-line    (:not posframe))
                                        (consult-ripgrep (:not posframe)))))
(use-package marginalia
  :init 
  (marginalia-mode 1))

(use-package consult
  :bind (("C-x b"   . consult-buffer)
	     ("C-x C-b" . consult-buffer)
	     ("C-c r"   . consult-ripgrep))
  :config
  (setq consult-preview-key (list :debounce 0.1 'any)))

(use-package which-key
  :ensure t
  :config (which-key-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package dashboard
  :config
  (setq dashboard-projects-backend 'project-el
        dashboard-startup-banner 'logo
        dashboard-banner-logo-title nil
        dashboard-center-content t
        dashboard-page-separator "\n\n\n"
        dashboard-items '((projects  . 15)
                          (recents   . 10)
                          (bookmarks . 5)))
  (dashboard-setup-startup-hook))
(use-package undo-fu
  :defer t
  :bind (("C-_" . undo-fu-only-undo)
         ("M-_" . undo-fu-only-redo)))
(use-package company
  :config
  (setq company-idle-delay                 0.0
        company-minimum-prefix-length      3
        company-tooltip-align-annotations  t
        company-tooltip-annotation-padding 1
        company-tooltip-margin             1
        company-detect-icons-margin        'company-dot-icons-margin)
  (global-company-mode t))
(use-package flycheck
 :defer t
 :init (global-flycheck-mode)
 :config (setq flycheck-display-errors-function #'ignore))
(use-package evil-nerd-commenter
  :defer t
  :bind ( ("C-ø" . evilnc-comment-or-uncomment-lines)))


