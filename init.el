;; -*- lexical-binding: t -*-
;;; init.el --- Emacs Configuration
;;; Commentary:
;; This config start here
;;; Code:

(require 'cl-lib)

;(require 'esup) ; emacs config profiling
(when (eval-when-compile (version< emacs-version "27"))
    (load "~/.emacs.d/early-init.el"))
   ; (package-initialize)
   ;)

   ;)

;(unless (package-installed-p 'use-package)
;    (package-refresh-contents)
;    (package-install 'use-package t))

(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 1)
;(setq straight-check-for-modifications '(check-on-save find-when-checking))

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

(use-package use-package :straight t
:custom (use-package-compute-statistics t)
)

(use-package use-package-ensure-system-package :after use-package :straight t)

(use-package el-patch :straight t)

;(use-package auto-package-update :straight t
;:custom (auto-package-update-delete-old-versions t)
;        (auto-package-update-prompt-before-update t)
;        ;(auto-package-update-hide-results t)
;:config (auto-package-update-maybe)
;)

(use-package org :straight t
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

;(setq-default custom-file "~/.emacs.d/custom-variable.el")
;(when (file-exists-p custom-file) (load-file custom-file))



(setq-default private-config-file "~/GoogleDrive/config/emacs-private-config.el")
(when (file-exists-p private-config-file) (load-file private-config-file))

;(garbage-collect)
;(put 'narrow-to-region 'disabled nil)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-ex-substitute-matches ((t (:inherit diff-removed :foreground unspecified :background unspecified))))
 '(evil-ex-substitute-replacement ((t (:inherit diff-added :foreground unspecified :background unspecified))))
 '(evil-traces-change ((t (:inherit diff-removed))))
 '(evil-traces-copy-preview ((t (:inherit diff-added))))
 '(evil-traces-copy-range ((t (:inherit diff-changed))))
 '(evil-traces-delete ((t (:inherit diff-removed))))
 '(evil-traces-global-match ((t (:inherit diff-added))))
 '(evil-traces-global-range ((t (:inherit diff-changed))))
 '(evil-traces-join-indicator ((t (:inherit diff-added))) t)
 '(evil-traces-join-range ((t (:inherit diff-changed))))
 '(evil-traces-move-preview ((t (:inherit diff-added))))
 '(evil-traces-move-range ((t (:inherit diff-removed))))
 '(evil-traces-normal ((t (:inherit diff-changed))))
 '(evil-traces-shell-command ((t (:inherit diff-changed))))
 '(evil-traces-substitute-range ((t (:inherit diff-changed))))
 '(evil-traces-yank ((t (:inherit diff-changed))))
 '(org-pomodoro-mode-line ((t (:foreground "#ff5555"))))
 '(org-pomodoro-mode-line-break ((t (:foreground "#50fa7b")))))
