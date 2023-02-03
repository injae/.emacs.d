;;; init.el --- Emacs Configuration
;; -*- lexical-binding: t -*-
;;; Commentary:
;; This config start here
;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
          (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
         (bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
                "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
                'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(eval-when-compile
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/straight/build/use-package"))
    (require 'use-package))


(use-package straight
    :custom (straight-use-package-by-default t))

;; (use-package esup) ; emacs config profiling
(use-package use-package)

(use-package gcmh :disabled
    :functions gcmh-mode
    :config (gcmh-mode t))

(use-package use-package-ensure-system-package)

(use-package exec-path-from-shell
    :functions exec-path-from-shell-initialize
    :config (exec-path-from-shell-initialize)
    )

(use-package asdf :straight (:host github :repo "tabfugnic/asdf.el")
    :after exec-path-from-shell
    :config (asdf-enable)
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

(use-package module-lisp-util :straight nil
    :config
    ;; Emacs 기본설정
    (load-modules-with-list "~/.emacs.d/module/"
        '( ;; emacs modules
             "emacs" "font" "evil"
             "git" "grep-util" "extension"
             "project-manage" "completion" "window"
             "buffer" "ui" "org"
             "terminal" "edit" "flycheck"
             "search" "multi-mode" "util"
             ))
    ;; programming 설정
    (load-modules-with-list "~/.emacs.d/module/prog/"
        '( ;; programming modules
             "lsp" "snippet" "highlight"
             "prog-search" "doc" "ssh"
             "coverage" "copilot" "tools"
             ;; language support
             "cpp" "lisp" "csharp"
             "rust" "haskell" "python"
             "flutter" "web" "ruby"
             "jvm" "go" "nix"
             "config-file" "docker"
             "formatting"
             ))
    )

;; 개인 설정
(defvar private-config-file "~/.emacs.d/private/token.el")
(when (file-exists-p private-config-file)
    (load-file private-config-file))

(use-package filenotify :straight nil :after org
    :preface
    (defvar env-org-file (expand-file-name "~/.emacs.d/env.org"))
    (defun update-env-org-file (event)
        (funcall-interactively 'org-babel-tangle-file env-org-file))
    :config
    (file-notify-add-watch env-org-file
        '(change attribute-change) 'update-env-org-file)
    )
;;; init.el ends here
