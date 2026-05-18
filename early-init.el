<<<<<<< HEAD
<<<<<<< HEAD
;;; early-init.el --- Early Startup File for Emacs Configuration of Eric Haberstroh -*- lexical-binding: t -*-

;; Copyright (C) 2023 Eric Haberstroh <eric@erichaberstroh.de>

;; Author: Eric Haberstroh <eric@erichaberstroh.de>
;; Created: 2023-07-25
;; Homepage: https://github.com/pille1842/dotemacs
;; Keywords: init
;; Package-Requires: ((emacs "28.2"))
;; SPDX-License-Identifier: GPL
;; Version: 0.1.0

;;; Commentary:

;; This file is loaded early in the startup process of Emacs.

;;; Code:

;; -------- PACKAGE MANAGEMENT --------

(setq package-enable-at-startup nil)

;;; early-init.el ends here
=======
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setenv "LIBRARY_PATH" "/opt/homebrew/lib/gcc/current:/opt/homebrew/lib/gcc/current/gcc/aarch64-apple-darwin24/15")
>>>>>>> 69277aa (Initial commit)
=======
;;; early-init.el --- Emacs Early Initialization File -*- lexical-binding: t -*-

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

;; This file's job is to do early configuration steps which must be done
;; before any frames in the graphical environment are created.

;;; Code:

(add-to-list 'initial-frame-alist '(fullboth . fullboth))
(setenv "LIBRARY_PATH" "/opt/homebrew/lib/gcc/current:/opt/homebrew/lib/gcc/current/gcc/aarch64-apple-darwin24/15")

;;; early-init.el ends here
>>>>>>> 321bdef (Add license and basic commentary to all files)
