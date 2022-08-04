;;; config.el --- Emacs Configuration
;;; -*- lexical-binding: t -*-
;;; Commentary:
;;; This config start here
;;; Code:
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
(load-file (expand-file-name "~/.emacs.d/module/lisp-util.el"))
(setq user-full-name "InJae Lee")
(setq user-mail-address "8687lee@gmail.com")

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/module/")
;(add-to-list 'load-path "~/.emacs.d/module/prog/")
(add-to-list 'load-path "~/.emacs.d/private/")

(setq-default custom-file "~/.emacs.d/custom-variable.el")
(when (file-exists-p custom-file) (load-file custom-file))

;; 개인 설정
(setq-default private-config-file "~/.emacs.d/private/token.el")
(when (file-exists-p private-config-file) (load-file private-config-file))

;;; emacs 기본설정
(load-modules-with-list "~/.emacs.d/module/" '(
    "emacs"
    "font"
    "evil" ;;; evil and keymap
    "git"
    "grep-util"
    "extension"
    "project_manage"
    "completion"
    "window"
    "buffer"
    "ui" ;;; pretty ui
    "org"
    "terminal"
    "edit" ;; edit (indent,,,)
    "flycheck"
    "search"
    "multi_mode"
    "util"
))

;;; programming 설정
(load-modules-with-list "~/.emacs.d/module/prog/" '(
    "lsp"
    "snippet"
    "highlight"
    "search"
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
    "config"
    "docker"
    ))

;;;
