;;; init.el --- Emacs Initialization File -*- lexical-binding: t -*-

;; Copyright (C) 2025 Eric Haberstroh

;; Author: Eric Haberstroh <eric@erichaberstroh.de>
;; Maintainer: Eric Haberstroh <eric@erichaberstroh.de>
;; Created: 2025
;; Version: 1.0
;; Package-Requires: ((emacs "30.2") (compat "30"))
;; URL: https://github.com/pille1842/dotemacs
;; Keywords: initialization, configuration

;; This file is NOT part of GNU Emacs.

;; Emacs Configuration © 2025 by Eric Haberstroh is licensed under CC BY-SA 4.0.
;; To view a copy of this license, visit:
;; <https://creativecommons.org/licenses/by-sa/4.0/>

;;; Commentary:

;; This file's job is to initialize the Emacs package system and then load
;; the actual configuration, which is stored as literate code blocks in
;; config.org.

;;; Code:

(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(org-babel-load-file (concat user-emacs-directory "config.org"))

;;; init.el ends here

