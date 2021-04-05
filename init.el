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

;; The purpose of this file is to prepare Emacs for loading the actual
;; configuration, which resides in erix-emacs.org in this directory.
;;
;; See the repo: https://github.com/pille1842/dotemacs

;;; Code:

;; Initialize the package system and use-package
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p "use-package")
  (package-install 'use-package))
(setq use-package-always-ensure t)

;; Prepare loading erix-emacs.org
(defvar erix-emacs-configuration-main-file "erix-emacs"
  "Base name of the main configuration file.")

(defun erix-emacs--expand-file-name (file extension)
  "Return canonical path to FILE with EXTENSION."
  (expand-file-name
   (concat user-emacs-directory file extension)))

(defun erix-emacs-load-config ()
  "Load main Emacs configuration, either '.el' or '.org' file."
  (let* ((main-init erix-emacs-configuration-main-file)
	 (main-init-el (erix-emacs--expand-file-name main-init ".el"))
	 (main-init-org (erix-emacs--expand-file-name main-init ".org")))
    (require 'org)
    (if (file-exists-p main-init-el)
	(load-file main-init-el)
      (when (file-exists-p main-init-org)
	(org-babel-load-file main-init-org)))))

(erix-emacs-load-config)

(declare-function org-babel-tangle-file "ob-tangle")

(defun erix-emacs-build-config ()
  "Produce Emacs Lisp init from the Org dotemacs file.
Add this to `kill-emacs-hook', to use the newest file in the next
session. The idea is to reduce startup time, though just by
rolling it over to the end of a session rather than the beginning
of it."
  (let* ((main-init erix-emacs-configuration-main-file)
	 (main-init-el (erix-emacs--expand-file-name main-init ".el"))
	 (main-init-org (erix-emacs--expand-file-name main-init ".org")))
    (when (file-exists-p main-init-el)
      (delete-file main-init-el))
    (require 'org)
    (when (file-exists-p main-init-org)
      (org-babel-tangle-file main-init-org main-init-el))))

(add-hook 'kill-emacs-hook #'erix-emacs-build-config)

;;; init.el ends here
