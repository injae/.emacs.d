;;; emacs-options.el --- initialization emacs
;;; Commentry:
;;; emacs options file 
;;; Code:

;; window-system options
(when window-system
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (if (boundp 'fringe-mode)     (fringe-mode -1))
    (if (boundp 'scroll-bar-mode) (scroll-bar-mode -1))
    (tooltip-mode -1)
    ;; 마우스 사용가능
    (xterm-mouse-mode)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 한글 글꼴 설정              ;;
;; 1234 5678 9012345678901234  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; text utf-8 setting
(set-language-environment "Korean")
(prefer-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(set-face-attribute 'default nil        :family "DejaVu Sans Mono" :height 100)
(set-fontset-font nil 'hangul (font-spec :family "D2Coding" :pixelsize 18))
(setq face-font-rescale-alist '(("D2coding" . 1.2)))
(setq-default line-spacing 6)
(global-font-lock-mode t)

;; emacs backround 
(set-frame-parameter nil 'alpha 0.95)
(setq compilation-window-height 15)

(set-variable 'cursor-type 'bar)
;; No popup frame
(setq pop-up-frames nil)

(setq ring-bell-function 'ignore)

; 버퍼 스위치
(winner-mode t)

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-splash-screen t)

(setq echo-keystrokes 0.5)
(setq global-hl-line-mode +1)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Esc 눌러서 버퍼 탈출 
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
