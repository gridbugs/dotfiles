;; Don't display the splash screen
(setq inhibit-startup-message t
      visible-bell t)

;; Display line numbers in every buffer
(global-display-line-numbers-mode 1)

;; Disable some unused UI elements 
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Allow the mouse when running in a terminal
(xterm-mouse-mode 1)

;; Use a line as a cursor
(setq-default cursor-type 'bar)

;; Use a dark theme
(load-theme 'modus-vivendi t)

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

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap
  (setq lsp-keymap-prefix "C-c l")
  :hook ((tuareg-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))
(use-package git-gutter-fringe)

;; The remainder of this file is automatically added by package installers.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(git-gutter-fringe git-gutter tuareg lsp-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
