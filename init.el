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
;(use-package straight :custom (straight-use-package-by-default t))

;(require 'esup) ; emacs config profiling

(load-file "~/.emacs.d/config.el")
;;;
