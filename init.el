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

(add-to-list 'load-path (expand-file-name"~/.emacs.d/straight/build"))

(eval-when-compile
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/straight/build/use-package"))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/straight/build/use-package-ensure-system-package"))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/straight/build/general"))
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/straight/build/bind-key"))
    (require 'use-package)
    (require 'use-package-ensure-system-package)
    (require 'general)
    (require 'bind-key)
    )

(straight-use-package 'use-package)
(use-package straight
    :custom (straight-use-package-by-default t)
            (straight-fix-flycheck t))

;(require 'esup) ; emacs config profiling
(use-package use-package)
(use-package gcmh :defer t :config (gcmh-mode t))
(use-package gcmh
    :hook (after-init . gcmh-mode))


(use-package use-package-ensure-system-package :straight t)
(use-package exec-path-from-shell :straight (:build (:not compile))
:preface (require 'use-package)
:config (exec-path-from-shell-initialize)
        ;(exec-path-from-shell-copy-env "PATH")
)

;;; font Setting
;; +------------+------------+
;; | 일이삼사오 | 일이삼사오 |
;; +------------+------------+
;; | ABCDEFGHIJ | ABCDEFGHIJ |
;; +------------+------------+
;; | 1234567890 | 1234567890 |
;; +------------+------------+
;; | abcdefghij | abcdefghij |
;; +------------+------------+

(setq user-full-name "InJae Lee")
(setq user-mail-address "8687lee@gmail.com")

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/module/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/module/prog/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/private/"))

(setq-default custom-file "~/.emacs.d/custom-variable.el")
(when (file-exists-p custom-file) (load-file custom-file))

(require 'module-lisp-util)
;;; Emacs 기본설정
(load-modules-with-list "~/.emacs.d/module/" '(
    "emacs"
    "font"
    "evil" ;;; evil and keymap
    "git"
    "grep-util"
    "extension"
    "project-manage"
    "completion"
    "window"
    "buffer"
    "ui" ;;; pretty ui
    "org"
    "terminal"
    "edit" ;; edit (indent,,,)
    "flycheck"
    "search"
    "multi-mode"
    "util"
))

;; 개인 설정
(defvar private-config-file "~/.emacs.d/private/token.el")
(setq-default private-config-file "~/.emacs.d/private/token.el")

;;; programming 설정
(load-modules-with-list "~/.emacs.d/module/prog/" '(
    "lsp"
    "snippet"
    "highlight"
    "prog-search"
    "doc"
    "ssh"
    "coverage"
    "copilot"
    "tools"
    ;; language
    "cpp"
    "lisp"
    "csharp"
    "rust"
    "haskell"
    "python"
    "flutter"
    "web"
    "ruby"
    "jvm"
    "go"
    "nix"
    "config-file"
    "docker"
))

(when (file-exists-p private-config-file) (load-file private-config-file))

(require 'server)
(unless (server-running-p) (server-start))

;;; init.el ends here
