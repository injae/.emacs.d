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

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;(add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

(require 'cl-lib)

;(setq straight-use-package-by-default t)
;(setq straight-vc-git-default-clone-depth 1)
;(setq straight-check-for-modifications '(check-on-save find-when-checking))

;(defvar bootstrap-version)
;(let ((bootstrap-file
;       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;      (bootstrap-version 5))
;  (unless (file-exists-p bootstrap-file)
;    (with-current-buffer
;        (url-retrieve-synchronously
;         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;         'silent 'inhibit-cookies)
;      (goto-char (point-max))
;      (eval-print-last-sexp)))
;  (load bootstrap-file nil 'nomessage))
;
;
;(straight-use-package 'use-package)

(use-package use-package :ensure t
:custom (use-package-compute-statistics t)
)

(use-package use-package-ensure-system-package :after use-package :ensure t)

(use-package el-patch :ensure t)

(use-package auto-package-update :ensure t
:custom (auto-package-update-delete-old-versions t)
        (auto-package-update-prompt-before-update t)
        ;(auto-package-update-hide-results t)
:config (auto-package-update-maybe)
)

;(toggle-debug-on-error)
;(setq byte-compile-error-on-warn t)

(use-package async :ensure t
:config (setq async-bytecomp-package-mode t)
)

(use-package org :ensure t
:mode (("\\.org\\'" . org-mode))
:init (org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
)

;(defun tangle-byte-compile-config-org-file ()
; "Org Tangle Speed up to use emacs-lisp byte compile config.org -> config.elc"
;    (interactive)
;    (when (equal (buffer-file-name) (expand-file-name "config.org" user-emacs-directory))

;        (byte-compile-file    (expand-file-name "tconfig.el" user-emacs-directory))))
;
;(add-hook 'after-save-hook #'tangle-byte-compile-config-org-file)
;(add-hook 'kill-emacs-hook #'tangle-byte-compile-config-org-file)
;
;(load-file (expand-file-name "tconfig.elc" user-emacs-directory))

; same work this code -> (org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
;(use-package literate-elisp :ensure t :pin melpa
;:config (literate-elisp-load (expand-file-name "config.org" user-emacs-directory))
;)

(setq-default custom-file "~/.emacs.d/custom-variable.el")
(when (file-exists-p custom-file) (load-file custom-file))


;(setq-default private-config-file "~/GoogleDrive/config/emacs-private-config.el")
(setq-default private-config-file "~/.emacs.d/private/token.el")
(when (file-exists-p private-config-file) (load-file private-config-file))

;(garbage-collect)
;(put 'narrow-to-region 'disabled nil)

(use-package async :ensure t
:config (setq async-bytecomp-package-mode t)
)

; for native comp
(setq package-native-compile t)
(setq comp-deferred-compilation t)
;(setq comp-deferred-compilation-deny-list '("powerline" "poly-mode"))
;(native-compile-async "~/.emacs.d/")
(native-compile-async "~/.emacs.d/config.el")


(defconst dd/using-native-comp-p (fboundp 'native-comp-available-p))
(when dd/using-native-comp-p
  (setq comp-async-query-on-exit t)
  (setq comp-async-jobs-number 4)
  (setq comp-async-report-warnings-errors nil)
  (setq comp-deferred-compilation t))


