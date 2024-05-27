(require 'server)
(unless (server-running-p)
  (server-start))

;; Don't display the splash screen
(setq inhibit-startup-message t)

(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(blink-cursor-mode 0)

;; Disable some unused UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Allow the mouse when running in a terminal
(xterm-mouse-mode 1)

;; Remember the position in files between sessions
(save-place-mode 1)

;; This lets us navigate the history of window positions
(winner-mode 1)

; Wrap long lines
(global-visual-line-mode t)

; Treat "_" as part of words by default in all code buffers
(defun my-modify-syntax-hook ()
  (modify-syntax-entry ?_ "w"))
(add-hook 'prog-mode-hook 'my-modify-syntax-hook)

; Always kill the current buffer without showing the menu when "C-x k" is pressed
(global-set-key [(control x) (k)] 'kill-this-buffer)

; Copy some similar keybindings from tmux
(global-set-key (kbd "C-x \"") 'split-window-below)
(global-set-key (kbd "C-x %") 'split-window-right)

;; This disables emacs's warning whehn openning a symlink to a file under vcs
(setq vc-handled-backends nil)

;; Use a line as a cursor
(setq-default cursor-type 'bar)

;; Show cursor column as well as line
(setq column-number-mode t)

;; Highlight whitespace at the end of lines
(defun my-enable-trailing-whitespace ()
  "Enable `show-trailing-whitespace' only for buffers visiting files."
  (when (and buffer-file-name
             (file-regular-p buffer-file-name))
    (setq show-trailing-whitespace t)))
(add-hook 'find-file-hook 'my-enable-trailing-whitespace)

;; Where to save autosave files
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "autosaves") t)))

;; Where to save backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Use default font to render text in markdown code blocks. this face
;; is also used for lsp-help buffers.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error ((t (:background "red4" :underline nil))))
 '(flycheck-hint ((t (:background "green4" :underline nil))))
 '(flycheck-info ((t (:background "blue4" :underline nil))))
 '(flycheck-warning ((t (:background "DarkGoldenrod4" :underline nil))))
 '(flyspell-duplicate ((t (:background "green4" :underline nil))))
 '(flyspell-incorrect ((t (:background "OrangeRed4" :underline nil))))
 '(helm-selection ((t (:background "gray30" :foreground "white" :weight bold))))
 '(helm-source-header ((t (:inherit font-lock-type-face :height 1.5 :weight bold))))
 '(lsp-headerline-breadcrumb-path-error-face ((t (:background "red4" :underline nil))))
 '(lsp-headerline-breadcrumb-path-hint-face ((t (:background "green4" :underline nil))))
 '(lsp-headerline-breadcrumb-path-info-face ((t (:background "blue4" :underline nil))))
 '(lsp-headerline-breadcrumb-path-warning-face ((t (:background "DarkGoldenrod4" :underline nil))))
 '(lsp-headerline-breadcrumb-symbols-error-face ((t (:background "red4" :underline nil))))
 '(lsp-headerline-breadcrumb-symbols-hint-face ((t (:background "green4" :underline nil))))
 '(lsp-headerline-breadcrumb-symbols-info-face ((t (:background "blue4" :underline nil))))
 '(lsp-headerline-breadcrumb-symbols-warning-face ((t (:background "DarkGoldenrod4" :underline nil))))
 '(markdown-code-face ((t (:inherit default))))
 '(markdown-inline-code-face ((t (:inherit default)))))

;; Uncomment to print backtrace on error (to the Backtrace buffer)
;(setq debug-on-error t)

;; Replace the terminal history functions with just sending the escape sequence.
;; This forces the emacs terminal emulators to respect the settings in inputrc.
(defun my-term-history-backward ()
  "Custom function for navigating backward through the history in term mode."
  (interactive)
  (setq term-buffer-maximum-size 0)
  (term-send-raw-string "\e[A"))
(defun my-term-history-forward ()
  "Custom function for navigating forward through the history in term mode."
  ;; Customize this function as needed
  (interactive)
  (setq term-buffer-maximum-size 0)
  (term-send-raw-string "\e[B"))
(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "<up>") 'my-term-history-backward)
            (define-key term-raw-map (kbd "<down>") 'my-term-history-forward)))

;; Load the extra config file if it exists
(setq extra-config-file "~/.emacs.d/extra.el")
(if (file-exists-p extra-config-file)
    (load extra-config-file))

;; Load cram mode directly from a file
(load "~/.emacs.d/cram-mode.el")

;; This is needed by the magit package
(setq package-install-upgrade-built-in t)

(require 'package)
(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  ;; only fetch the archives if you don't have use-package installed
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
	use-package-expand-minimally t))
(require 'use-package)

(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'mocha))

(defun my-helm-customizations ()
  "Set custom font sizes for Helm faces."
  (custom-set-faces
   '(helm-source-header
     ((t (:inherit font-lock-type-face :height 1.5 :weight bold))))
   '(helm-selection
     ((t (:background "gray30" :foreground "white" :weight bold))))))

(defun reload-theme ()
  (interactive)
  (progn
    (set-variable 'frame-background-mode 'dark)
    (disable-theme `modus-operandi)
    (load-theme 'modus-vivendi t)
    (if (display-graphic-p)
	(progn
	  (setq catppuccin-flavor 'mocha)
	  (load-theme 'catppuccin t)
	  (my-helm-customizations)
	  (message "Using graphical theme"))
      (progn
	(disable-theme 'catppuccin)
	(set-background-color "color-232")
	(message "Using terminal theme")
	))))
(reload-theme)
(global-set-key (kbd "C-c x x") 'reload-theme)

(defun light-theme ()
  (interactive)
  (progn
    (set-variable 'frame-background-mode 'light)
    (disable-theme 'modus-vivendi)
    (load-theme 'modus-operandi)
    (setq catppuccin-flavor 'latte)
    (catppuccin-reload)
    ))

(use-package tuareg
  :custom
  (tuareg-opam-insinuate t)
  :config)

(use-package rustic
  :config
  (setq lsp-eldoc-hook nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-signature-auto-activate nil)
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))
(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

(use-package ocamlformat)
(add-hook 'before-save-hook 'ocamlformat-before-save)

(use-package dune-format)
(use-package dune)
(add-hook 'dune-mode-hook
          (lambda ()
            (dune-format-on-save-mode)))

(use-package flycheck
  :init (global-flycheck-mode))
(global-set-key (kbd "M-n") 'flycheck-next-error)
(global-set-key (kbd "M-p") 'flycheck-previous-error)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  :hook ((tuareg-mode . lsp))
  :commands lsp
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     '("opam" "exec" "--" "ocamllsp"))
    :major-modes '(caml-mode tuareg-mode reason-mode)
    :server-id 'ocamllsp)))
(setq lsp-inlay-hint-enable t)
(setq lsp-signature-auto-activate t)

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :hook (ledger-mode .git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

;; Display line numbers in every buffer
(global-display-line-numbers-mode 1)
(use-package magit
  :config
  (setq magit-define-global-key-bindings 'recommended))

(use-package helm
  :config (helm-mode 1))
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
;; Prevent the current window from being maximized while displaying the helm buffer
(setq helm-display-function #'helm-default-display-buffer)
(setq helm-split-window-default-side 'below)
(setq helm-split-window-in-side-p t)

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (define-key global-map (kbd "C-.") 'company-files))

(use-package company-quickhelp
  :config
  (setq company-quickhelp-delay 0)
  (company-quickhelp-mode))

(use-package which-key
  :config (which-key-mode))

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode +1)
  (def-projectile-commander-method ?t
    "Run named-ansi-term in project."
      (named-ansi-term)))
(setq projectile-switch-project-action 'projectile-commander)
(use-package helm-projectile
  :config (helm-projectile-on))

;; A simpler undo system that persists across runs (also needed for evil mode)
(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z") 'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))
(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))
(undo-fu-session-global-mode)

;; Needed for some navigation commands in evil mode
(use-package goto-chg)

(setq evil-undo-system 'undo-fu)
(use-package evil
  :config
  (evil-mode 1))
(evil-set-initial-state 'term-mode 'emacs)
; Match the behaviour of Alt+: in vim (same as pressing ':' in normal mode)
(global-unset-key (kbd "M-:"))
(global-set-key (kbd "M-:") 'evil-ex)
;; Revert Ctrl-a and Ctrl-e to Emacs defaults in Evil mode
(define-key evil-normal-state-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
(define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance)
(define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat)
(define-key evil-insert-state-map (kbd "S-<left>") 'windmove-left)
(define-key evil-insert-state-map (kbd "S-<right>") 'windmove-right)
(define-key evil-insert-state-map (kbd "S-<up>") 'windmove-up)
(define-key evil-insert-state-map (kbd "S-<down>") 'windmove-down)
(define-key evil-normal-state-map (kbd "S-<left>") 'windmove-left)
(define-key evil-normal-state-map (kbd "S-<right>") 'windmove-right)
(define-key evil-normal-state-map (kbd "S-<up>") 'windmove-up)
(define-key evil-normal-state-map (kbd "S-<down>") 'windmove-down)
(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)

(defun set-cursor-shape (shape)
  "Set the cursor shape using terminal escape sequences."
  (when (eq window-system 'nil)
    (send-string-to-terminal (format "\033[%d q" shape))))
(add-hook 'evil-normal-state-entry-hook (lambda () (set-cursor-shape 2)))
(add-hook 'evil-insert-state-entry-hook (lambda () (set-cursor-shape 6)))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package terminal-toggle)
(setq terminal-toggle--term-shell (getenv "SHELL"))
(global-set-key (kbd "C-x w") 'terminal-toggle)

(use-package vimrc-mode)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package inheritenv)

; NB ledger-mode can't use envrc to locate the "ledger" binary. The
; calls to inheritenv-add-advice are an attempt to fix this but they
; don't work. They're left here in case I want to try to fix this one
; day. For now I'm fixing this by installing ledger into my nix
; profile.
(use-package ledger-mode)
(add-hook 'ledger-mode-hook #'company-mode)
(inheritenv-add-advice 'ledger-init-load-init-file)
(inheritenv-add-advice 'ledger-version-greater-p)
(inheritenv-add-advice 'ledger-read-commodity-with-prompt)
(inheritenv-add-advice 'ledger-add-transaction)
(inheritenv-add-advice 'ledger-texi-invoke-command)
(inheritenv-add-advice 'ledger-display-balance-at-point)
(inheritenv-add-advice 'ledger-display-ledger-stats)
(inheritenv-add-advice 'ledger-payees-list)
(inheritenv-add-advice 'ledger-accounts-list)
(inheritenv-add-advice 'ledger-report-expand-format-specifiers)
(inheritenv-add-advice 'ledger-reconcile-get-cleared-or-pending-balance)


(use-package flymake
  :bind (("C-c e" . flymake-show-project-diagnostics)))

(use-package sh-script
  :hook (sh-mode . flymake-mode))

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(use-package yaml-mode)
(use-package toml-mode)

(add-hook 'lua-mode-hook #'lsp)
(setq lua-indent-level 2)
(add-hook 'lua-mode-hook
          (lambda ()
            (setq tab-width 2)))

(add-hook 'python-mode-hook #'lsp)
(setq python-indent-offset 2)

(defun rename-buffer-unique-with-suffix (base-name &optional suffix-count)
  "Rename the current buffer appending a suffix to disambiguate.
NAME is the name af the new buffer.
SUFFIX-COUNT is the first integer suffix to try
  (it will be incremented until the name is unique)."
  (let ((name (if suffix-count
		  (concat base-name " (" (number-to-string suffix-count) ")")
		base-name)))
    (if (get-buffer name)
	(let ((suffix-count (if suffix-count (+ suffix-count 1) 1)))
	  (rename-buffer-unique-with-suffix base-name suffix-count))
      (rename-buffer name))))

(defun rename-ansi-term-buffer ()
  "Rename the \"ansi-term\" buffer to match the current working directory."
  (let* ((default-directory (expand-file-name default-directory))
         (buffer-name (concat "*ansi-term " default-directory "*")))
    (rename-buffer-unique-with-suffix buffer-name)))

(defun named-ansi-term ()
  "Start a new \"ansi-term\" with a buffer named after the current working directory."
  (interactive)
  (let* ((bash-homebrew "/opt/homebrew/bin/bash")
	 (bash-nixos "/run/current-system/sw/bin/bash")
	 (bash-default "/usr/bin/env bash")
	 (bash (cond
		((file-exists-p bash-homebrew) bash-homebrew)
		((file-exists-p bash-nixos) bash-nixos)
		(t bash-default))))
    (ansi-term bash)
    (rename-ansi-term-buffer)))

(defun then-rename-terminal (&rest args)
  "Call ORIG with ARGS, then rename the terminal to its cwd."
  (rename-ansi-term-buffer))

(global-set-key (kbd "C-c n t") 'named-ansi-term)
(advice-add 'cd :after #'then-rename-terminal)

; Kill a terminal's buffer when the terminal exits
(defadvice term-handle-exit
  (after term-kill-buffer-on-exit activate)
  (kill-buffer))

(use-package evil-mc)
(global-evil-mc-mode  1)
(evil-define-key 'visual evil-mc-key-map
  "A" #'evil-mc-make-cursor-in-visual-selection-end
  "I" #'evil-mc-make-cursor-in-visual-selection-beg)

(use-package org-tree-slide
  :config
  (org-tree-slide-slide-in-effect-toggle)
  (define-key org-tree-slide-mode-map (kbd "<f9>") 'org-tree-slide-move-previous-tree)
  (define-key org-tree-slide-mode-map (kbd "<f10>") 'org-tree-slide-move-next-tree)
  (define-key org-mode-map (kbd "<f8>") 'org-tree-slide-mode)
  (define-key org-mode-map (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle))

;; Window navigation using arrow keys.  It's convenient to use shift
;; as the modifier on macos but it's more convenient to use control on
;; linux, so juts bind both.
(global-set-key (kbd "S-<left>") 'windmove-left)
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<up>") 'windmove-up)
(global-set-key (kbd "S-<down>") 'windmove-down)

;; The remainder of this file is automatically added by package installers.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-tree-slide toml-mode evil-mc yasnippet envrc multiple-cursors yaml-mode flymake-shellcheck rustic ledger-mode company-quickhelp flycheck exec-path-from-shell vimrc-mode ocamlformat terminal-toggle nix-mode evil goto-chg seq helm-projectile projectile which-key company helm magit git-gutter lsp-mode dune-format tuareg catppuccin-theme use-package))
 '(windmove-default-keybindings '([ignore] meta control)))


(provide 'init)
;;; init.el ends here
