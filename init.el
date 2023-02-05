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

(use-package use-package)
(use-package use-package-ensure-system-package)

(use-package straight :custom (straight-use-package-by-default t))

(use-package no-littering
:config (require 'recentf)
        (add-to-list 'recentf-exclude no-littering-var-directory)
        (add-to-list 'recentf-exclude no-littering-etc-directory)
        (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
        (when (fboundp 'startup-redirect-eln-cache)
            (startup-redirect-eln-cache
                (convert-standard-filename
                    (expand-file-name  "var/eln-cache/" user-emacs-directory))))
)

(use-package exec-path-from-shell
    :functions exec-path-from-shell-initialize
    :config (exec-path-from-shell-initialize)
    )

(use-package asdf :straight (:host github :repo "tabfugnic/asdf.el")
    :after exec-path-from-shell
    :config (asdf-enable)
    )

(setq user-full-name "InJae Lee")
(setq user-mail-address "8687lee@gmail.com")

;; custom lisp library
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))

(setq-default custom-file "~/.emacs.d/custom-variable.el")
(when (file-exists-p custom-file) (load-file custom-file))

(use-package +lisp-util :load-path "module/" :straight nil
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
             "run-command"
             ))
    ;; programming 설정
    (load-modules-with-list "~/.emacs.d/module/prog/"
        '( ;; programming modules
             "tree-sitter" "lsp" "snippet"
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
(add-to-list 'load-path (expand-file-name "~/.emacs.d/private/"))
(defvar private-config-file "~/.emacs.d/private/token.el")
(when (file-exists-p private-config-file)
    (load-file private-config-file))

(use-package filenotify :straight nil :after org
    :ensure-system-package (watchexec . "cargo install watchexec-cli")
    :preface
    (defvar env-org-file (expand-file-name "~/.emacs.d/env.org"))
    (defun update-env-org-file (event)
        (funcall-interactively 'org-babel-tangle-file env-org-file))
    :config
    (file-notify-add-watch env-org-file
        '(change attribute-change) 'update-env-org-file)
    )
;;; init.el ends here
