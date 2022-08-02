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
;(require 'use-package)
(use-package use-package :straight t
:custom (use-package-compute-statistics t)
)
(use-package use-package-ensure-system-package :straight t :after use-package)

(use-package exec-path-from-shell :straight t
:config (exec-path-from-shell-initialize)
        (exec-path-from-shell-copy-env "PATH")
)

(load-file "~/.emacs.d/config.el")

;;;
