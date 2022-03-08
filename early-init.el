;;; early-init.el --- early bird  -*- no-byte-compile: t -*-
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
;(setq straight-use-package-by-default t)

(require 'package)
(add-to-list 'package-archives '("melpa"        . "https://melpa.org/packages/")         t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/")  t)
(add-to-list 'package-archives '("gnu"          . "https://elpa.gnu.org/packages/")      t)
(add-to-list 'package-archives '("org"          . "http://orgmode.org/elpa/")            t)

;;
