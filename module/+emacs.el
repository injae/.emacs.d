;;; +emacs.el --- Summery
;;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar *is-mac*     (eq system-type 'darwin))
(defvar *is-windows* (eq system-type 'windows-nt))
(defvar *is-cygwin*  (eq system-type 'cygwin))
(defvar *is-linux*   (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(defvar *is-wsl*     (eq (string-match "Linux.*microsoft.*WSL2.*Linux" (shell-command-to-string "uname -a")) 0))
(defvar *is-unix*    (or *is-linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)))

(use-package emacs
    :init
    (setq ad-redefinition-action 'accept)
    (setq max-lisp-eval-depth 10000)
    ;(setq debug-on-error t) ; debug option
    ;; emacs gc setting
    (setq gc-cons-threshold (* 100 1000000)) ;; emacs speed up setting in 16GB RAM
    (setq read-process-output-max (* 1024 1024)) ;; 1mb
    ;(run-with-idle-timer 5 t 'garbage-collect)

    ;; scroll bar
    (scroll-bar-mode -1)
    (setq scroll-step 1)
    (setq scroll-conservatively 10000)

    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (tooltip-mode -1)
    (xterm-mouse-mode)

    ;(set-frame-parameter nil 'alpha 0.95)
    (set-variable 'cursor-type '(hbar . 10))

    ; no # ~ file
    (setq create-lockfiles nil)
    (setq make-backup-files nil)

    ;; No popup frame
    (setq pop-up-frames nil)
    (setq ring-bell-function 'ignore)
    ; layout save setting
    (winner-mode t)
    ;(desktop-save-mode 1)
    (setq frame-resize-pixelwise t) ; emacs plus fullscreen bugfix option
    (setq inhibit-startup-message t)
    (setq inhibit-startup-echo-area-message t)
    (setq inhibit-splash-screen t)
    (setq echo-keystrokes 0.5)
    (setq global-hl-line-mode +1)
    (defalias 'yes-or-no-p 'y-or-n-p)
    (global-auto-revert-mode)
    
    ;; Enable recursive minibuffers
    (setq enable-recursive-minibuffers t)

    (setq completion-cycle-threshold 3)

    (setq tab-always-indent 'complete)
)

(use-package ns-auto-titlebar
:if *is-mac*
:config (ns-auto-titlebar-mode)
        (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
        (add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; assuming you are using a dark theme
        (setq frame-title-format nil)
)

(use-package wsl-setting :straight nil :no-require t
:if *is-wsl*
:config
    (defconst powershell-exe "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe")
    (when (file-executable-p powershell-exe)
        (defun my\wsl-browse-url (url &optional _new-window)
            "Opens link via powershell.exe"
            (interactive (browse-url-interactive-arg "URL: "))
            (let ((quotedUrl (format "start '%s'" url)))
            (apply 'call-process powershell-exe
                    nil 0 nil (list "-Command" quotedUrl))))

        (setq-default browse-url-browser-function 'my\wsl-browse-url))
        (setq frame-resize-pixelwise t)
        (pixel-scroll-precision-mode)
)

(use-package not-wsl-setting :straight nil :no-require t
:unless *is-wsl*
:config (set-frame-parameter nil 'alpha 0.95)
)
;; emacs large file setting
(use-package so-long-mode :straight nil :no-require t
;;; default text parsing direction left -> right
:if (version<= "27.1" emacs-version)
:config
    (setq bidi-paragraph-direction 'left-to-right)
    (setq bidi-inhibit-bpa t)
    (global-so-long-mode 1)

)

(use-package pixel-scoll-smooth :straight nil :no-require t
;; default text parsing direction left -> right
:if (version<= "29" emacs-version)
:config (pixel-scroll-precision-mode)
)

;; emacs debug utils
;(use-package esup)
;(use-package bug-hunter)

(use-package explain-pause-mode :disabled
    :straight (:type git :host github :repo "lastquestion/explain-pause-mode")
    :config (explain-pause-mode)
)
;; (setq warning-minimum-level :error)

(use-package gc-buffers
    :config (gc-buffers-mode)
)

(use-package gcmh
    :functions gcmh-mode
    :config (gcmh-mode t))

(use-package helpful
:bind (("C-h f" . helpful-callable)
       ("C-h v" . helpful-variable)
       ("C-h k" . helpful-key))
)

(use-package with-editor
    :hook ((shell-mode  . with-editor-export-editor)
           (eshell-mode . with-editor-export-editor)
           ;; (vterm-mode  . with-editor-export-editor) in .zshrc
           (term-mode   . with-editor-export-editor))
    :config
    (define-key (current-global-map)
        [remap async-shell-command] 'with-editor-async-shell-command)
    (define-key (current-global-map)
        [remap shell-command] 'with-editor-shell-command)
    )

(provide '+emacs)
;;; +emacs.el ends here
