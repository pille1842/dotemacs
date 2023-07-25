;;; init.el --- Emacs Configuration of Eric Haberstroh -*- lexical-binding: t -*-

;; Copyright (C) 2023 Eric Haberstroh <eric@erichaberstroh.de>

;; Author: Eric Haberstroh <eric@erichaberstroh.de>
;; Created: 2023-07-25
;; Homepage: https://github.com/pille1842/dotemacs
;; Keywords: init
;; Package-Requires: ((emacs "28.2"))
;; SPDX-License-Identifier: GPL
;; Version: 0.1.0

;;; Commentary:

;; This file configures GNU Emacs to my liking.

;;; Code:

;; -------- PACKAGE MANAGEMENT --------

;; Bootstrap straight.el (Reference: <https://github.com/radian-software/straight.el>)
(setq straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Load use-package
(straight-use-package 'use-package)

;; -------- USER INTERFACE --------

;; Hide toolbar, scrollbars and menubar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Load modus-vivendi theme
(use-package modus-themes
  :straight
  (modus-themes :type git :host github :repo "protesilaos/modus-themes")
  :config
  (load-theme 'modus-vivendi :no-confirm))

;; Start Emacs with a maximized frame. Reference: <https://emacsredux.com/blog/2020/12/04/maximize-the-emacs-frame-on-startup/>
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; Show line numbers and column numbers everywhere
(column-number-mode 1)
(global-display-line-numbers-mode 1)

;; -------- FONTS --------

;; Load fontaine package and configure fonts
;; This configuration relies on the "Iosevka Comfy" font family. Reference: <https://github.com/protesilaos/iosevka-comfy>
;; Copy the fonts from the iosevka-comfy/ttf directory to ~/.local/share/fonts/ and regenerate the font cache
;; with fc-cache -f -v
(use-package fontaine
  :straight
  (fontaine :type git :host github :repo "protesilaos/fontaine")
  :config
  (setq fontaine-presets
	'((regular
	   :default-family "Iosevka Comfy"
	   :default-weight regular
	   :default-height 140)))
  (fontaine-set-preset 'regular))

;; -------- GENERAL EDITING COMMANDS --------

;; Sensible defaults
(use-package sensible-defaults
  :straight
  (sensible-defaults :type git :host github :repo "hrs/sensible-defaults.el")
  :config
  ;; Settings
  (sensible-defaults/increase-gc-threshold)
  (sensible-defaults/delete-trailing-whitespace)
  (sensible-defaults/treat-camelcase-as-separate-words)
  (sensible-defaults/automatically-follow-symlinks)
  (sensible-defaults/make-scripts-executable)
  (sensible-defaults/single-space-after-periods)
  (sensible-defaults/apply-changes-to-highlighted-region)
  (sensible-defaults/overwrite-selected-text)
  (sensible-defaults/ensure-that-files-end-with-newline)
  (sensible-defaults/quiet-startup)
  (sensible-defaults/make-dired-file-sizes-human-readable)
  (sensible-defaults/shorten-yes-or-no)
  (sensible-defaults/always-highlight-code)
  (sensible-defaults/show-matching-parens)
  (sensible-defaults/set-default-line-length-to 120)
  (sensible-defaults/yank-to-point-on-mouse-click)
  (sensible-defaults/backup-to-temp-directory)
  ;; Keybindings
  (sensible-defaults/bind-commenting-and-uncommenting)
  (sensible-defaults/bind-home-and-end-keys)
  (sensible-defaults/bind-keys-to-change-text-size))

;; -------- VERSION CONTROL --------

;; Install Magit
(use-package magit
  :straight
  (magit :type git :host github :repo "magit/magit")
  :config
  (setq magit-define-global-key-bindings 'default))

;;; init.el ends here
