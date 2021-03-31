;;; init.el --- Emacs configuration
;;
;; Copyright (C) 2021 Eric Haberstroh
;;
;; Filename:    init.el
;; Description: Configure Emacs to my taste
;; Author:      Eric Haberstroh <gpg@erixpage.de>
;; Homepage:    https://github.com/pille1842/dotemacs
;; Version:     1.0.0
;; Keywords:    initialization startup
;; GitHub:      https://github.com/pille1842/dotemacs

;; This file is not part of GNU Emacs

;;; License:
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see
;; <https://www.gnu.org/licenses>.

;;; Commentary:

;; This is my initialization file. There are many like it, but this
;; one is mine.

;;; Code:

;; Don't save customizations in this init file. The custom-file will
;; be loaded at the end
(custom-set-default 'custom-file (concat user-emacs-directory "custom.el"))

;; Default font size
(defvar eh/default-font-size 120 "Default font size for this system")

;; Clean up the UI
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(setq visible-bell t
      inhibit-splash-screen t
      frame-title-format '(multiple-frames "%b"
					   "Emacs"))

;; Font and cursor
(set-face-attribute 'default nil :font "Fira Code Retina" :height eh/default-font-size)
(setq-default cursor-type 'bar)

;; Authentication sources
(setq auth-sources '("~/.authinfo.gpg"))
(customize-set-variable 'ange-ftp-netrc-filename "~/.authinfo.gpg")

;; Initialize package system
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")
			 ("elpa"  . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Configure use-package
(unless (package-installed-p "use-package")
  (package-install 'use-package))
(setq use-package-always-ensure t)

;; Theme: doom-dracula from https://github.com/hlissner/emacs-doom-themes
(use-package doom-themes
  :init (load-theme 'doom-dracula t))

;; Enable ligatures from https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
(use-package fira-code-mode
  :hook prog-mode)

;; doom-modeline
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :custom
  ((doom-modeline-height 15)))

;; Line numbers, column numbers and current line
(global-display-line-numbers-mode 1)
(column-number-mode 1)
(global-hl-line-mode 1)

;; Rainbow delimiters (colorful parentheses)
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; No line numbers in certain modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))

;; Smooth scrolling (keep more context when moving the cursor)
(use-package smooth-scrolling
  :init
  (smooth-scrolling-mode 1))

;; Completion framework
(use-package ivy
  :init
  (ivy-mode 1))

;; Enhanced versions of common Emacs commands
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-h b" . counsel-descbinds)
	 ("C-h a" . counsel-apropos)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

;; More annotations in completion lists
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; Swiper for text search
(use-package swiper
  :bind (("C-s" . swiper)
	 ("C-r" . swiper-backward)))

;; which-key for discoverability
(use-package which-key
  :init
  (which-key-mode 1))

;; Better Emacs *Help* buffer
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-callable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Orgmode
(defun eh/org-mode-setup ()
  "Set up various things for Orgmode buffers"
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun eh/org-font-setup ()
  "Set up font rendering in Orgmode buffers"
  (font-lock-add-keywords 'org-mode
			  '(("^ *\\([-]\\) "
			     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

   ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "DejaVu Sans" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :ensure org-plus-contrib
  :hook (org-mode . eh/org-mode-setup)
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :config
  (setq org-ellipsis " ▾")
  (eh/org-font-setup)
  (setq org-agenda-files '("~/org")))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun eh/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . eh/org-mode-visual-fill))

;; Project management
(use-package projectile
  :config
  (projectile-mode 1)
  :custom
  ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Code")
    (setq projectile-project-search-path '("~/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit)

;; Treemacs
(use-package treemacs
  :bind
  (("M-0" . treemacs-select-window)
   ("C-x t 1" . treemacs-delete-other-windows)
   ("C-x t t" . treemacs)
   ("C-x t B" . treemacs-bookmark)
   ("C-x t C-t" . treemacs-find-file)
   ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit))

;; Development

;; Enable electric-pair-mode in all prog-mode buffers
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; Language Server Protocol (LSP)
(use-package lsp-mode
  :hook ((php-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c x"))

;; LSP enhancements
(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; PHP
(use-package php-mode)

;; Use <pause> key to save the current buffer
(global-set-key (kbd "<pause>")
		'save-buffer)

;; Load the custom-file if it exists
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
