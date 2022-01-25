;;; init.el --- Emacs Configuration
;; -*- lexical-binding: t -*-
;;; Commentary:
;; This config start here
;;; Code:


;(require 'esup) ; emacs config profiling
(when (eval-when-compile (version< emacs-version "27"))
    (load "~/.emacs.d/early-init.el");)
    (package-initialize)
   )

(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package t))

;(eval-when-compile
;  ;; Following line is not needed if use-package.el is in ~/.emacs.d
;  ;(add-to-list 'load-path "<path where use-package is installed>")
;  (require 'use-package))

(require 'use-package)
(require 'cl-lib)

(use-package use-package :ensure t
:custom (use-package-compute-statistics t)
)

; for native comp
(setq package-native-compile t)
(setq comp-deferred-compilation t)
(setq-default comp-deferred-compilation-deny-list '("powerline" "polymode-core" "cc-mode" "progmodes" "cc-engine"))
;(setq comp-deferred-compilation-deny-list '("powerline" "poly-mode"))
;(native-compile-async "~/.emacs.d/")
;(async-byte-compile-file "~/.emacs.d/config.el")
(native-compile-async "~/.emacs.d/config.el")
(load-file "~/.emacs.d/config.el")
;;;
