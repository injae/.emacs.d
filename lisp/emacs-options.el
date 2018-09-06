;;; emacs-options.el --- initialization emacs
;;; Commentry:
;;; emacs options file 
;;; Code:

;; window-system options
(when window-system
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (tooltip-mode -1)

    ;; default window size options
    (if (display-graphic-p)
        (progn
        (setq initial-frame-alist
                '(
                (tool-bar-lines . 0)
                (width . 200) ; chars
                (height . 60) ; lines
                (left . 100)
                (top . 60)))
        (setq default-frame-alist
                '(
                (tool-bar-lines . 0)
                (width . 200)
                (height . 60)
                (left . 100)
                (top . 60))))
    (progn
        (setq initial-frame-alist '( (tool-bar-lines . 0)))
        (setq default-frame-alist '( (tool-bar-lines . 0)))))
)

(setq scroll-step 1)
(setq scroll-conservatively 10000)

;; # backup file auto make option 
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq make-backup-files nil)

;; text utf-8 setting
(set-language-environment "Korean")
(prefer-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; emacs backround 
(set-frame-parameter nil 'alpha 0.95)

(set-variable 'cursor-type 'bar)

;; No popup frame
(setq ns-pop-up-frames nil)
(setq pop-up-frames nil)

;(setq visible-bell t) 
(global-font-lock-mode t)

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-splash-screen t)

(setq echo-keystrokes 0.5)
(setq global-hl-line-mode +1)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
