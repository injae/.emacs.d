;;; config.el --- Emacs Configuration
;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; This config start here
;;; Code:
;; profile
(use-package use-package)

(use-package use-package-ensure-system-package :straight t)
(use-package exec-path-from-shell :straight t 
:config (exec-path-from-shell-initialize)
        ;(exec-path-from-shell-copy-env "PATH")
)

(load-file (expand-file-name "~/.emacs.d/module/lisp-util.el"))

(setq user-full-name "InJae Lee")
(setq user-mail-address "8687lee@gmail.com")

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/module/")
(add-to-list 'load-path "~/.emacs.d/private/")

(setq-default custom-file "~/.emacs.d/custom-variable.el")
(when (file-exists-p custom-file) (load-file custom-file))

;; 개인 설정
(setq-default private-config-file "~/.emacs.d/private/token.el")
(when (file-exists-p private-config-file) (load-file private-config-file))

;;; emacs 기본설정
(load-file (expand-file-name "~/.emacs.d/module/emacs.el"))
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
(load-file (expand-file-name "~/.emacs.d/module/font.el"))

;;; evil and keymap
(load-file (expand-file-name "~/.emacs.d/module/evil.el"))

;; git
(load-file (expand-file-name "~/.emacs.d/module/git.el"))

;;; grep
(load-file (expand-file-name "~/.emacs.d/module/grep-util.el"))

;;; extension 
(load-file (expand-file-name "~/.emacs.d/module/extension.el"))

;;; project manager
(load-file (expand-file-name "~/.emacs.d/module/project.el"))

;; completion
(load-file (expand-file-name "~/.emacs.d/module/completion.el"))

;; window manage
(load-file (expand-file-name "~/.emacs.d/module/window.el"))

;; buffer manage
(load-file (expand-file-name "~/.emacs.d/module/buffer.el"))

;;; Pretty UI
(load-file (expand-file-name "~/.emacs.d/module/ui.el"))

;; org-mode
(load-file (expand-file-name "~/.emacs.d/module/org.el"))

;; terminal
(load-file (expand-file-name "~/.emacs.d/module/terminal.el"))

;; edit (indent,,,)
(load-file (expand-file-name "~/.emacs.d/module/edit.el"))

;; flycheck
(load-file (expand-file-name "~/.emacs.d/module/flycheck.el"))

;; search
(load-file (expand-file-name "~/.emacs.d/module/search.el"))

;; multi mode
(load-file (expand-file-name "~/.emacs.d/module/multi_mode.el"))

;; utils
(load-file (expand-file-name "~/.emacs.d/module/util.el"))

;; programming
(load-file (expand-file-name "~/.emacs.d/module/prog/lsp.el"))
(load-file (expand-file-name "~/.emacs.d/module/prog/snippet.el"))
(load-file (expand-file-name "~/.emacs.d/module/prog/highlight.el"))
(load-file (expand-file-name "~/.emacs.d/module/prog/search.el"))
(load-file (expand-file-name "~/.emacs.d/module/prog/doc.el"))

(load-file (expand-file-name "~/.emacs.d/module/prog/ssh.el"))
(load-file (expand-file-name "~/.emacs.d/module/prog/coverage.el"))
(load-file (expand-file-name "~/.emacs.d/module/prog/copilot.el"))

(load-file (expand-file-name "~/.emacs.d/module/prog/docker.el"))
(load-file (expand-file-name "~/.emacs.d/module/prog/cpp.el"))
(load-file (expand-file-name "~/.emacs.d/module/prog/lisp.el"))
(load-file (expand-file-name "~/.emacs.d/module/prog/cshap.el"))
(load-file (expand-file-name "~/.emacs.d/module/prog/rust.el"))
(load-file (expand-file-name "~/.emacs.d/module/prog/haskell.el"))
(load-file (expand-file-name "~/.emacs.d/module/prog/python.el"))
(load-file (expand-file-name "~/.emacs.d/module/prog/flutter.el"))
(load-file (expand-file-name "~/.emacs.d/module/prog/web.el"))
(load-file (expand-file-name "~/.emacs.d/module/prog/ruby.el"))
(load-file (expand-file-name "~/.emacs.d/module/prog/jvm.el"))
(load-file (expand-file-name "~/.emacs.d/module/prog/go.el"))
(load-file (expand-file-name "~/.emacs.d/module/prog/config.el"))
(load-file (expand-file-name "~/.emacs.d/module/prog/tools.el"))
    
(provide 'config)
;;; config.el
