;;; init.el --- Emacs Configuration
;; -*- lexical-binding: t -*-
;;; Commentary:
;; This config start here
;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;(require 'esup) ; emacs config profiling
(require 'use-package)
(use-package use-package :straight t
:custom (use-package-compute-statistics t)
)

(load-file "~/.emacs.d/config.el")
;;;


;(require 'cl-lib)

;(unless (package-installed-p 'use-package)
;    (package-refresh-contents)
;    (package-install 'use-package t))
;(eval-when-compile
;  ;; Following line is not needed if use-package.el is in ~/.emacs.d
;  ;(add-to-list 'load-path "<path where use-package is installed>")
;  (require 'use-package))
;(use-package auto-compile :ensure t
;:init
;    (setq load-prefer-newer t)
;    (setq auto-compile-mode-line-counter t)
;:config
;    (auto-compile-on-load-mode)
;    (auto-compile-on-save-mode)
;)
;
;(use-package auto-package-update :ensure t
;:custom (auto-package-update-delete-old-versions t)
;        (auto-package-update-prompt-before-update t)
;        ;(auto-package-update-hide-results t)
;:config (auto-package-update-maybe)
;)
