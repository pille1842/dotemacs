;;; init.el --- Emacs initialization file -*- lexical-binding: t -*-

;; Copyright (C) 2021 Eric Haberstroh

;; Maintainer: gpg@erixpage.de
;; Keywords: startup init
;; Package: eh-emacs

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file configures Emacs.

;;; Code:

(setq inhibit-startup-message t)

(scroll-bar-mode -1)    ;; Disable visible scrollbar
(tool-bar-mode -1)      ;; Disable the toolbar
(tooltip-mode -1)       ;; Disable tooltips
(menu-bar-mode -1)      ;; Disable the menu bar
(set-fringe-mode 10)    ;; Give some breathing room

;; Set up the visible bell
(setq visible-bell t)

;; Use a better-looking font
(set-face-attribute 'default nil :font "Fira Code Retina" :height 120)

;; White background blinds me
(load-theme 'tango-dark t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Don't put customizations in this file
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Load my favorite theme
(use-package doom-themes
  :init (load-theme 'doom-dracula t))

;; Show column numbers in the modeline
(column-number-mode 1)

;; Show line numbers...
(global-display-line-numbers-mode 1)

;; ... but not in some major modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))

;; Configure searching
(use-package swiper)

;; Alternatives to some built-in commands
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history)))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

;; Make Ivy displays more helpful
(use-package ivy-rich
  :init (ivy-rich-mode 1))

;; More helpful help functions
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Configure the modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 30)))

;; More colorful parentheses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Show which keybindings are available
(use-package which-key
  :init (which-key-mode)
  :custom ((which-key-idle-delay 0.3)))

;; Pretty icons
(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

;; More convenient keybindings
(use-package general
  :config
  (general-create-definer ehmacs/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (ehmacs/leader-keys
   "t"  '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose theme")))

(defun ehmacs/evil-hook ()
  "Setup evil mode"
  (dolist (mode '(custom-mode
		  eshell-mode
		  git-rebase-mode
		  erc-mode
		  circe-server-mode
		  circe-chat-mode
		  circe-query-mode
		  sauron-mode
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

;; Become evil

(setq org-want-keybinding nil)

(use-package evil
  :custom
  (evil-want-integration t)
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump nil)
  :hook (evil-mode . ehmacs/evil-hook)
  :init
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Many heads
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(ehmacs/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(setq auth-sources '("~/.authinfo.gpg"))

;; Project management
(use-package projectile
  :config (projectile-mode)
  :custom (projectile-completion-system 'ivy)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Code")
    (setq projectile-project-search-path '("~/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode 1))

;; Version control
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; For this to work: https://magit.vc/manual/ghub.html#Creating-and-Storing-a-Token
(use-package forge
  :after magit)

;; Orgmode

(defun ehmacs/org-mode-setup ()
  "Various set up tasks for Orgmode"
  (org-indent-mode 1)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :ensure org-plus-contrib
  :hook (org-mode . ehmacs/org-mode-setup)
  :custom
  (org-agenda-files '("~/org"))
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
     (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
  (org-agenda-custom-commands
   '(("d" "Dashboard"
      ((agenda "" ((org-deadline-warning-days 7)))
       (todo "NEXT"
             ((org-agenda-overriding-header "Next Tasks")))
       (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

     ("n" "Next Tasks"
      ((todo "NEXT"
             ((org-agenda-overriding-header "Next Tasks")))))

     ("W" "Work Tasks" tags-todo "+work-email")

     ;; Low-effort next actions
     ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
      ((org-agenda-overriding-header "Low Effort Tasks")
       (org-agenda-max-todos 20)
       (org-agenda-files org-agenda-files)))

     ("w" "Workflow Status"
      ((todo "WAIT"
             ((org-agenda-overriding-header "Waiting on External")
              (org-agenda-files org-agenda-files)))
       (todo "REVIEW"
             ((org-agenda-overriding-header "In Review")
              (org-agenda-files org-agenda-files)))
       (todo "PLAN"
             ((org-agenda-overriding-header "In Planning")
              (org-agenda-todo-list-sublevels nil)
              (org-agenda-files org-agenda-files)))
       (todo "BACKLOG"
             ((org-agenda-overriding-header "Project Backlog")
              (org-agenda-todo-list-sublevels nil)
              (org-agenda-files org-agenda-files)))
       (todo "READY"
             ((org-agenda-overriding-header "Ready for Work")
              (org-agenda-files org-agenda-files)))
       (todo "ACTIVE"
             ((org-agenda-overriding-header "Active Projects")
              (org-agenda-files org-agenda-files)))
       (todo "COMPLETED"
             ((org-agenda-overriding-header "Completed Projects")
              (org-agenda-files org-agenda-files)))
       (todo "CANC"
             ((org-agenda-overriding-header "Cancelled Projects")
              (org-agenda-files org-agenda-files)))))))
  (org-tag-alist
   '((:startgroup)
     ;; Put mutually exclusive tags here
     (:endgroup)
     ("@errand" . ?E)
     ("@home" . ?H)
     ("@work" . ?W)
     ("pvt" . ?t)
     ("pvg" . ?g)
     ("pvf" . ?f)
     ("pkb" . ?b)
     ("jet" . ?j)))
  (org-refile-targets
   '(("Archiv.org" :maxlevel . 2)
     ("Aufgaben.org" :maxlevel . 1)))
  (org-capture-templates
   '(("t" "Tasks / Projects")
     ("tt" "Task" entry (file+olp "~/org/Aufgaben.org" "Eingang")
      "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
     ("ts" "Clocked Entry Subtask" entry (clock)
      "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

     ("j" "Journal Entries")
     ("jj" "Journal" entry
      (file+olp+datetree "~/org/Tagebuch.org")
      "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
      :clock-in :clock-resume
      :empty-lines 1)
     ("jm" "Meeting" entry
      (file+olp+datetree "~/org/Tagebuch.org")
      "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
      :clock-in :clock-resume
      :empty-lines 1)
     ("m" "Metrics Capture")
     ("mw" "Weight" table-line (file+headline "~/org/Metrik.org" "Gewicht")
      "| %U | %^{Weight} | %^{Notizen} |" :kill-buffer t)))
     
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  :config
  (setq org-ellipsis " ▼"
	org-hide-emphasis-markers nil)
  ;; Save Org buffers after refiling
  (advice-add 'org-refile :after 'org-save-all-org-buffers))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(font-lock-add-keywords 'org-mode
			'(("^ *\\([-]\\) "
			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "·"))))))

(with-eval-after-load 'org-faces
  (dolist (face '((org-level-1 . 1.2)
		  (org-level-2 . 1.1)
		  (org-level-3 . 1.05)
		  (org-level-4 . 1.0)
		  (org-level-5 . 1.1)
		  (org-level-6 . 1.1)
		  (org-level-7 . 1.1)
		  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face))))

(require 'org-indent)

(with-eval-after-load 'org-faces
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-date nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-document-info-keyword nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-drawer nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-todo nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch)))

(defun ehmacs/org-mode-visual-fill ()
  "Set up visual-fill-mode for Orgmode buffers"
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . ehmacs/org-mode-visual-fill))

;;; init.el ends here
