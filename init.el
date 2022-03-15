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

(use-package auto-compile :ensure t
:init
    (setq load-prefer-newer t)
    (setq auto-compile-mode-line-counter t)
:config
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)
)

(use-package auto-package-update :ensure t
:custom (auto-package-update-delete-old-versions t)
        (auto-package-update-prompt-before-update t)
        ;(auto-package-update-hide-results t)
:config (auto-package-update-maybe)
)

; for native comp
(setq package-native-compile t)
;(setq comp-deferred-compilation t)
;(setq-default comp-deferred-compilation-deny-list '("powerline" "polymode-core" "cc-mode" "progmodes" "cc-engine"))
(native-compile-async "~/.emacs.d/init.el")
(native-compile-async "~/.emacs.d/config.el")
;(load-file "~/.emacs.d/init.el")
(load-file "~/.emacs.d/config.el")
;;;
