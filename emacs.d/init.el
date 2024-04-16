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

;; Load the extra config file if it exists
(setq extra-config-file "~/.emacs.d/extra.el")
(if (file-exists-p extra-config-file)
    (load extra-config-file))

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

(use-package company)

(setq neo-theme 'arrow)
(use-package neotree)
(global-set-key (kbd "C-x n t") 'neotree-toggle)

(use-package which-key
  :config (which-key-mode))

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode +1))
(use-package helm-projectile
  :config (helm-projectile-on))

;; The remainder of this file is automatically added by package installers.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(seq helm-projectile projectile which-key neotree company helm magit git-gutter-fringe git-gutter lsp-mode dune-format tuareg catppuccin-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
