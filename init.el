;;; init.el --- Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;; This config start here
;;; Code:

;(require 'esup) ; emacs config profiling
(when (eval-when-compile (version< emacs-version "27"))
    (load "~/.emacs.d/early-init.el")
    (package-initialize))

    (unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package t))

(use-package use-package :ensure t :pin melpa
:custom (use-package-compute-statistics t)
)

(use-package auto-package-update :ensure t :pin melpa
:custom (auto-package-update-delete-old-versions t)
        (auto-package-update-prompt-before-update t)
        ;(auto-package-update-hide-results t)
:config (auto-package-update-maybe)
)
(use-package use-package-ensure-system-package :ensure t :pin melpa)

;(use-package quelpa-use-package :ensure t :pin melpa :config (setq quelpa-update-melpa-p nil))
(use-package org :ensure t :pin melpa :mode ("\\.org\\'" . org-mode)
:init (org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
)

;(defun tangle-byte-compile-config-org-file ()
; "Org Tangle Speed up to use emacs-lisp byte compile config.org -> config.elc"
;    (interactive)
;    (when (equal (buffer-file-name) (expand-file-name "config.org" user-emacs-directory))
;        (org-babel-tangle nil (expand-file-name "tconfig.el" user-emacs-directory))
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



(setq-default private-config-file "~/GoogleDrive/config/emacs-private-config.el")
(when (file-exists-p private-config-file) (load-file private-config-file))

;(garbage-collect)
;(put 'narrow-to-region 'disabled nil)
;;; 


   ; (unless (and (package-installed-p 'delight)
   ;              (package-installed-p 'use-package))
   ;     (package-refresh-contents)
   ;     (package-install 'delight t)
   ;     (package-install 'use-package t))
