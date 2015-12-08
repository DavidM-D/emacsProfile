;;; brew stuff
(if (eq system-type 'darwin)
    (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
      (normal-top-level-add-subdirs-to-load-path))
)

;;; remove uneeded GUI
(tool-bar-mode -1)
(setq inhibit-startup-message t)

;;; package management
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-ghc-show-info t)
 '(custom-safe-themes
   (quote
    ("11636897679ca534f0dec6f5e3cb12f28bf217a527755f6b9e744bd240ed47e1", "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-tags-on-save t)
 '(js2-basic-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(scroll-bar ((t nil))))
;;; loads shared packages
(require 'auto-complete)
(require 'auto-complete-config)
(require 'ag)

;;; loads dependant files
(load-file "~/emacsProfile/theme.el")
(textmate-mode)

(require 'smartparens-config)
(smartparens-global-mode)
(show-smartparens-global-mode t)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
(setq make-backup-files nil)
(delete-selection-mode 1)
(require 'multiple-cursors)
(add-to-list 'load-path "~/emacsProfile")
(load "ruby.el")
(load "javascript.el")
(load "haskell-config.el") ;; renamed to avoid naming conflict with haskell vars
(load "keybindings.el")
(load "git.el")
(load "langfmt.el")
(load "utils.el")
(set-face-attribute 'default nil :height 105)

(setq backup-directory-alist `(("." . "~/.saves")))
(load "editorconfig")
(require 'go-mode-autoloads)
(scroll-bar-mode -1)
(global-linum-mode 1)
(winner-mode 1)
(menu-bar-mode -1)
(setq initial-scratch-message nil)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;;; path add
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
