;;; init.el --- Emacs Configuration
;; -*- lexical-binding: t -*-
;;; Commentary:
;; This config start here
;;; Code:

(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                          :ref nil
                          :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                          :build (:not elpaca--activate-package)))

(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
          (build (expand-file-name "elpaca/" elpaca-builds-directory))
          (order (cdr elpaca-order))
          (default-directory repo))
    (add-to-list 'load-path (if (file-exists-p build) build repo))
    (unless (file-exists-p repo)
        (make-directory repo t)
        (when (< emacs-major-version 28) (require 'subr-x))
        (condition-case-unless-debug err
            (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                        ((zerop (call-process "git" nil buffer t "clone"
                                    (plist-get order :repo) repo)))
                        ((zerop (call-process "git" nil buffer t "checkout"
                                    (or (plist-get order :ref) "--"))))
                        (emacs (concat invocation-directory invocation-name))
                        ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                    "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                        ((require 'elpaca))
                        ((elpaca-generate-autoloads "elpaca" repo)))
                (progn (message "%s" (buffer-string)) (kill-buffer buffer))
                (error "%s" (with-current-buffer buffer (buffer-string))))
            ((error) (warn "%s" err) (delete-directory repo 'recursive))))
    (unless (require 'elpaca-autoloads nil t)
        (require 'elpaca)
        (elpaca-generate-autoloads "elpaca" repo)
        (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
    (elpaca-use-package-mode)
    (setq elpaca-use-package-by-default t)
    )

(elpaca-wait)

;; (eval-when-compile
;;     ;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/straight/build/use-package"))
;;     (require 'use-package))

;; (use-package use-package)
;; (use-package use-package-ensure-system-package)
;; (use-package general :elpaca (:host github :repo "noctuid/general.el"))

(use-package no-littering
    :config (require 'recentf)
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)
    (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
    ;; (when (fboundp 'startup-redirect-eln-cache)
    ;;     (startup-redirect-eln-cache
    ;;         (convert-standard-filename
    ;;             (expand-file-name  "var/eln-cache/" user-emacs-directory))))
    )
(use-package exec-path-from-shell
    :custom (
                (exec-path-from-shell-variables '("PATH"
                                                  "MANPATH"
                                                  "TMPDIR"
                                                  "KUBECONFIG"
                                                  "LSP_USE_PLISTS"
                                                  "GOPRIVATE"
                                                  "RUST_BACKTRACE"
                                                  "MallocNanoZone"))
                (exec-path-from-shell-arguments '("-l"))
                (exec-path-from-shell-check-startup-files nil)
                (exec-path-from-shell-debug nil)
                )
    :config (exec-path-from-shell-initialize)
    )

(use-package direnv :disabled
    :after exec-path-from-shell
    :config (direnv-mode)
    )

(use-package envrc 
    :config (envrc-global-mode)
    )

(setq user-full-name "InJae Lee")
(setq user-mail-address "8687lee@gmail.com")

;; custom lisp library
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))

(setq-default custom-file "~/.emacs.d/custom-variable.el")
(when (file-exists-p custom-file) (load-file custom-file))

(use-package +lisp-util :elpaca nil :load-path "~/.emacs.d/module/")

(elpaca-wait)

(use-package module-util :elpaca nil :after (dash f s)
    :config
    ;; Emacs 기본설정
    (load-modules-with-list "~/.emacs.d/module/"
        '( ;; emacs modules
             emacs font evil
             git grep-util extension
             project-manage completion
             window   buffer  ui
             org terminal edit
             flycheck search
             multi-mode util
             run-command
             ))
    ;; programming 설정
    (load-modules-with-list "~/.emacs.d/module/prog/"
        '( ;; programming modules
             tree-sitter lsp snippet
             prog-search doc ssh
             coverage copilot tools
             ;; language support
             cpp lisp csharp
             rust python ;; haskell
             flutter web ruby
             jvm  go  nix lua
             config-file docker
             formatting bazel
             ))
    )

;; 개인 설정
(add-to-list 'load-path (expand-file-name "~/.emacs.d/private/"))
(defvar private-config-file "~/.emacs.d/private/token.el")
(when
    (file-exists-p private-config-file)
    (load-file private-config-file))

(use-package filenotify :elpaca nil :after (exec-path-from-shell org)
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
