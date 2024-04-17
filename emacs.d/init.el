;; Use a dark theme so the screen is dark until the real theme loads
(load-theme 'modus-vivendi t)

;; Don't display the splash screen
(setq inhibit-startup-message t
      visible-bell t)

;; Disable some unused UI elements 
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Allow the mouse when running in a terminal
(xterm-mouse-mode 1)

;; This disables emacs's warning whehn openning a symlink to a file under vcs
(setq vc-handled-backends nil)

;; Use a line as a cursor
(setq-default cursor-type 'bar)

(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory ".autosaves/") t)))

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

(if (display-graphic-p)
    (use-package catppuccin-theme
      :config (load-theme 'catppuccin :no-confirm)))

(use-package tuareg
  :custom
  (tuareg-opam-insinuate t)
  :config)

(use-package dune-format)
(use-package dune)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((tuareg-mode . lsp))
  :commands lsp
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     '("opam" "exec" "--" "ocamllsp"))
    :major-modes '(caml-mode tuareg-mode reason-mode)
    :server-id 'ocamllsp)))

(use-package direnv)

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))
(use-package git-gutter-fringe)

;; Display line numbers in every buffer
(global-display-line-numbers-mode 1)
(use-package magit)

(use-package helm
  :config (helm-mode 1))
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
;; Prevent the current window from being maximized while displaying the helm buffer
(setq helm-display-function #'pop-to-buffer)

(use-package company)

(setq neo-theme 'arrow)
(use-package neotree)
(global-set-key (kbd "C-x n t") 'neotree-toggle)
(setq neo-window-fixed-size nil)


(use-package which-key
  :config (which-key-mode))

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode +1)
  (def-projectile-commander-method ?t
    "Run ansi-term in project."
      (ansi-term (getenv "SHELL") (concat "ansi-term (" projectile-project-name ")"))))
(setq projectile-switch-project-action 'projectile-commander)
(use-package helm-projectile
  :config (helm-projectile-on))

;; A simpler undo system that persists across runs (also needed for evil mode)
(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))
(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))
(undo-fu-session-global-mode)

;; Needed for some navigation commands in evil mode
(use-package goto-chg)

(use-package evil
  :config
  (evil-mode 1))

;; The remainder of this file is automatically added by package installers.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil goto-chg seq helm-projectile projectile which-key neotree company helm magit git-gutter-fringe git-gutter lsp-mode dune-format tuareg catppuccin-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
