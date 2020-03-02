;;; init.el --- Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;; This config start here
;;; Code:
(setq gc-cons-threshold 100000000); emacs speed up setting in 16GB RAM
(when (eval-when-compile (version< emacs-version "27"))
    (load "~/.emacs.d/early-init.el")
    (package-initialize))

    (setq use-package-compute-statistics t)
    (unless (and (package-installed-p 'delight)
                 (package-installed-p 'use-package))
        (package-refresh-contents)
        (package-install 'delight t)
        (package-install 'use-package t))
    ;(setq-default use-package-always-defer t)

(use-package auto-package-update :ensure t :pin melpa
:init (setq auto-package-update-delete-old-versions t)
      (setq auto-package-update-hide-results t)
      (auto-package-update-maybe)
)

(use-package use-package-ensure-system-package :ensure t :pin melpa)

;(use-package quelpa-use-package :ensure t :pin melpa)
(use-package org :ensure t :pin melpa :mode ("\\.org\\'" . org-mode)
:init (org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
)

; same work this code -> (org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
;(use-package literate-elisp :ensure t :pin melpa
;:config (literate-elisp-load (expand-file-name "config.org" user-emacs-directory))
;)

(setq-default custom-file "~/.emacs.d/custom-variable.el")
(when (file-exists-p custom-file) (load-file custom-file))

;(garbage-collect)
;(put 'narrow-to-region 'disabled nil)
;;;
