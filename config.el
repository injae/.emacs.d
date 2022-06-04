;; -*- lexical-binding: t -*-
;;; config.el --- Emacs Configuration
;;; Commentary:
;; This config start here
;; Code:
;;;org-babel-load-file has bug
;;; #BEGIN_SRC emacs-lisp -> elisp

(require 'use-package)
;(use-package use-package :ensure t)
(use-package use-package-ensure-system-package :after use-package :ensure t)
(use-package el-patch :ensure t)
;(toggle-debug-on-error)
;(setq byte-compile-error-on-warn t)

(use-package async :ensure t
:config (setq async-bytecomp-package-mode t)
)

(use-package org :ensure t
:mode ("\\.org\\'" . org-mode)
)

(use-package org-modern :ensure t :disabled
:hook ((org-mode . org-modern-mode)
       (org-agenda-finalize . org-modern-agenda))
:config
    (setq
        ;; Edit settings
        org-auto-align-tags nil
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t

        ;; Org styling, hide markup etc.
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis "…"

        ;; Agenda styling
        org-agenda-block-separator ?─
        org-agenda-time-grid
        '((daily today require-timed)
             (800 1000 1200 1400 1600 1800 2000)
             " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────")
)

(setq-default custom-file "~/.emacs.d/custom-variable.el")
(when (file-exists-p custom-file) (load-file custom-file))


(setq-default private-config-file "~/.emacs.d/private/token.el")
(when (file-exists-p private-config-file) (load-file private-config-file))

;(garbage-collect)
;(put 'narrow-to-region 'disabled nil)
; for native comp
(setq package-native-compile t)
(setq comp-deferred-compilation t)
;(setq-default comp-deferred-compilation-deny-list '("powerline" "polymode-core" "cc-mode" "progmodes" "cc-engine"))
;(setq comp-deferred-compilation-deny-list '("powerline" "poly-mode"))
;(native-compile-async "~/.emacs.d/")

(setq user-full-name "Injae Lee")
(setq user-mail-address "8687lee@gmail.com")

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/private/")

(use-package emacs-gc-setting :no-require t
:init (setq gc-cons-threshold 100000000); emacs speed up setting in 16GB RAM
      (setq read-process-output-max (* 1024 1024))
      (run-with-idle-timer 2 t (lambda () (garbage-collect)))  ; 2초마다, repeat
)

(use-package esup :ensure t)



;(use-package bug-hunter :ensure t )
(use-package explain-pause-mode :disabled ;:ensure (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode")
:config (explain-pause-mode)
)

(setq ad-redefinition-action 'accept)
(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 32000)
;(setq debug-on-error t) ; debug option

(defvar *is-mac*     (eq system-type 'darwin))
(defvar *is-windows* (eq system-type 'windows-nt))
(defvar *is-cygwin*  (eq system-type 'cygwin))
(defvar *is-linux*   (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(defvar *is-wsl*     (eq (string-match "Linux.*microsoft.*WSL2.*Linux" (shell-command-to-string "uname -a")) 0))
(defvar *is-unix*    (or *is-linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)))

(use-package scroll-bar :no-require t
:if window-system
:init (scroll-bar-mode -1)
:config
    (setq scroll-step 1)
    (setq scroll-conservatively 10000)
)

(use-package tool-bar :no-require t
:if window-system
:init (tool-bar-mode -1)
)

(use-package menu-bar :no-require t
:if window-system
:init (menu-bar-mode -1)
)

(use-package tooltip-mode :no-require t
:if window-system
:init (tooltip-mode -1)
)

(use-package mouse :no-require t
:if window-system
:init (xterm-mouse-mode)
)

(use-package ns-auto-titlebar :ensure t
:if *is-mac*
:config (ns-auto-titlebar-mode)
        (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
        (add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; assuming you are using a dark theme
        (setq ns-use-proxy-icon nil)
        (setq frame-title-format nil)
)

(use-package wsl-setting :no-require t :ensure nil
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

(use-package not-wsl-setting :no-require t :ensure nil
:unless *is-wsl*
:config (set-frame-parameter nil 'alpha 0.95)
)

;(set-frame-parameter nil 'alpha 0.95)
(setq compilation-window-height 15)
(set-variable 'cursor-type '(hbar . 10))

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
;; emacs large file setting
(use-package so-long-mode :no-require t :ensure nil
;; default text parsing direction left -> right 
:if (version<= "27.1" emacs-version)
:config
    (setq bidi-paragraph-direction 'left-to-right)
    (setq bidi-inhibit-bpa t)
    (global-so-long-mode 1)
)

;(use-package pixel-scoll-smooth :no-require t :ensure nil
;;; default text parsing direction left -> right 
;:if (version<= "29" emacs-version)
;:config (pixel-scroll-precision-mode)
;)

;; +------------+------------+
;; | 일이삼사오 | 일이삼사오 |
;; +------------+------------+
;; | ABCDEFGHIJ | ABCDEFGHIJ |
;; +------------+------------+
;; | 1234567890 | 1234567890 |
;; +------------+------------+
;; | 일이삼사오 | 일이삼사오 |
;; | abcdefghij | abcdefghij |
;; +------------+------------+
;; text utf-8 setting
;(setq utf-translate-cjk-mode nil)
(set-language-environment "Korean")
(prefer-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq system-time-locale "C")

; some font use mode speed up config (ex: org-superstar)
(setq inhibit-compacting-font-caches t)
; NanumGothicCoding Setting
(set-face-attribute   'default            nil       :family "Fira Code" :height 130)
;(set-face-attribute   'default            nil       :family "FiraCode Nerd Font Mono" :height 130)
(set-fontset-font nil 'hangul            (font-spec :family "NanumGothicCoding"  :pixelsize 17))
(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "NanumGothicCoding"  :pixelsize 17))
(setq face-font-rescale-alist '(("NanumGothicCoding" . 1.2)))

; D2Coding Setting
;(set-face-attribute   'default            nil       :family "Fira Code" :height 120)
;(set-fontset-font nil 'hangul            (font-spec :family "D2Coding"  :pixelsize 18))
;(set-fontset-font nil 'japanese-jisx0208 (font-spec :family "D2Coding"  :pixelsize 18))
;(setq face-font-rescale-alist '(("D2coding" . 1.17)))

;(set-face-attribute   'default            nil       :family "FiraCode Nerd Font Mono" :height 120)
;(setq face-font-rescale-alist '(("D2coding" . 1.03877)))
;(setq face-font-rescale-alist '(("D2coding" . 0.85)))
(when *is-mac*
    (progn
        (require 'ucs-normalize)
        (set-file-name-coding-system 'utf-8-hfs)
        (setq default-process-coding-system '(utf-8-hfs . utf-8-hfs))
        (set-terminal-coding-system  'utf-8-hfs)
        ))

(setq-default line-spacing 3)

(global-font-lock-mode t)

;; 한글입력할때 완성전까지 안보이는 문제 해결을 위해 내장 한글입력기 사용
; Linux 내장 한글입력기 사용법
; ~/.Xresources 만들고 그안에 Emacs*useXIM: false 입력
; 터미널에 xrdb ~/.Xresources 하고 xrdb -merge ~/.Xresources 하고 이맥스 다시키면 됨
(setq default-input-method "korean-hangul")
(setq default-korean-keyboard 'korean-hangul)
;(global-set-key [S-SPC] 'toggle-input-method) ; Ivy모드를 사용하면 S-SPC를 ivy-minibuffer-map에서 remapping 해줘야 한다.
(global-set-key [?\S- ] 'toggle-input-method) ; Ivy모드를 사용하면 S-SPC를 ivy-minibuffer-map에서 remapping 해줘야 한다.
(global-set-key (kbd "S-SPC") 'toggle-input-method) ; Ivy모드를 사용하면 S-SPC를 ivy-minibuffer-map에서 remapping 해줘야 한다.
(global-set-key (kbd "<f17>") 'toggle-input-method) ; macos shift-space setting Karabiner를 사용해야된다.
(global-set-key (kbd "<Hangul>") 'toggle-input-method)

(use-package restart-emacs :ensure t)
(defun launch-separate-emacs-in-terminal () (suspend-emacs "fg ; emacs -nw"))
(defun launch-separate-emacs-under-x () (call-process "sh" nil nil nil "-c" "emacs &"))
(defun -restart-emacs ()
    ;; We need the new emacs to be spawned after all kill-emacs-hooks
    ;; have been processed and there is nothing interesting left
    (interactive)
    (let ((kill-emacs-hook (append kill-emacs-hook (list (if (display-graphic-p) #'launch-separate-emacs-under-x
                                                                                 #'launch-separate-emacs-in-terminal)))))
            (save-buffers-kill-emacs))
)

(defun -reload-emacs ()
    (interactive)
    (load-file (expand-file-name "~/.emacs.d/config.el"))
)

;(use-package paradox :ensure t :disabled
;;https://github.com/Malabarba/paradox
;:commands (package-list-packages)
;:config (paradox-enable)
;)

(use-package default-text-scale :ensure t 
:config (default-text-scale-mode)
        ;(if *is-wsl* (default-text-scale-increment 20))
        ;(if *is-wsl* (default-text-scale-increment 45))
)

(use-package textsize :load-path "lisp/textsize")

(use-package drag-stuff :ensure t  :defer t
:after evil
:init (drag-stuff-global-mode t)
        (drag-stuff-define-keys)
)

(use-package server :config (unless (server-running-p) (server-start)))

;(setq warning-minimum-level :error)

; large date blob read

(setq read-process-output-max (* 1024 1024)) ; 1mb
  
(defun new-buffer-save (name buffer-major-mode)
    (interactive)
    (let ((buffer (generate-new-buffer name)))
         (switch-to-buffer buffer)
         (set-buffer-major-mode buffer)
         (funcall buffer-major-mode)
         (setq buffer-offer-save t))
)

(defun new-buffer (name buffer-major-mode)
    (interactive)
    (let ((buffer (generate-new-buffer name)))
         (switch-to-buffer buffer)
         (set-buffer-major-mode buffer)
         (funcall buffer-major-mode))
)

(defun new-no-name-buffer ()
    (interactive)
    (new-buffer "untitled" 'text-mode)
)

(use-package hungry-delete :ensure t :disabled

; 공백 지울때 한꺼번에 다지워짐
:init (global-hungry-delete-mode)
)

(use-package face-picker :no-require t :ensure nil :disabled
:preface
(defun what-face (pos)
     (interactive "d")
     (let ((face (or (get-char-property (pos) 'read-face-name)
                     (get-char-property (pos) 'face))))
          (if face (message "Face: %s" face) (message "No face at %d" pos))))
)

; text random
(defun randomize-region (beg end)
(interactive "r")
(if (> beg end)
    (let (mid) (setq mid end end beg beg mid)))
(save-excursion
    ;; put beg at the start of a line and end and the end of one --
    ;; the largest possible region which fits this criteria
    (goto-char beg)
    (or (bolp) (forward-line 1))
    (setq beg (point))
    (goto-char end)
    ;; the test for bolp is for those times when end is on an empty
    ;; line; it is probably not the case that the line should be
    ;; included in the reversal; it isn't difficult to add it
    ;; afterward.
    (or (and (eolp) (not (bolp)))
        (progn (forward-line -1) (end-of-line)))
    (setq end (point-marker))
    (let ((strs (shuffle-list
                (split-string (buffer-substring-no-properties beg end)
                            "\n"))))
    (delete-region beg end)
    (dolist (str strs)
        (insert (concat str "\n"))))))

(defun shuffle-list (list)
"Randomly permute the elements of LIST.
All permutations equally likely."
(let ((i 0) j temp
    (len (length list)))
        (while (< i len)
        (setq j (+ i (random (- len i))))
        (setq temp (nth i list))
        (setcar (nthcdr i list) (nth j list))
        (setcar (nthcdr j list) temp)
        (setq i (1+ i))))
    list)

(use-package modern-fringes :ensure t :defer t
:config (modern-fringes-invert-arrows)
        (modern-fringes-mode)
)

;(use-package composite 
;:defer t
;;:if (version<= "27.0" emacs-version) 
;:hook (emacs-lisp-mode)
;:config
;    (let ((alist '((?λ . ,(regexp-opt '("lambda"))
;                  ))))
;         (dolist (char-regexp alist)
;             (set-char-table-range composition-function-table (car char-regexp)
;                                   `([,(cdr char-regexp) 0 font-shape-gstring]))))
;)

(use-package keypression :ensure t 
:commands keypression-mode
:custom (keypression-use-child-frame t)
        (keypression-fade-out-delay 1.0)
        (keypression-frame-justify 'keypression-left-fringe)
        (keypression-cast-command-name t)
        (keypression-cast-coommand-name-format "%s  %s")
        (keypression-frame-background-mode 'white)
        (keypression-combine-same-keystrokes t)
        (keypression-frames-maxnum 20)
        (keypression-font-face-attribute '(:width normal :height 200 :weight bold))
)

(use-package evil :ensure t 
:custom (evil-want-keybinding nil)
:init   (setq evil-want-integration t)
:config (setq evil-want-C-u-scroll t)
        (setq evil-symbol-word-search t)
        ;(define-key evil-normal-state-map (kbd "q") 'nil)
        (define-key evil-visual-state-map (kbd "R") 'evil-visual-exchange-corners)
        (evil-ex-define-cmd "k" 'kill-this-buffer)
        (setq-default evil-kill-on-visual-paste nil)
        ;(fset 'evil-visual-update-x-selection 'ignore) ; visual mode 'p' command update clipboard problem fix
        (evil-mode 1)
)

(use-package move-text :ensure t :after evil
:bind (:map evil-visual-state-map
            ("C-j" . drag-stuff-down)
            ("C-k" . drag-stuff-up  ))
:config (move-text-default-bindings)
)

(use-package general :ensure t 
:after evil
:init (setq general-override-states '(insert emacs  hybrid   normal
                                      visual motion override operator replace))
:config
      (general-evil-setup :with-shortname-maps)
      (general-create-definer leader :keymaps '(global override) :states '(n v) :prefix "SPC")
      (leader "<SPC>" 'counsel-M-x
              "e"     '(:wk "Emacs")
              "b"     '(:wk "Buffer")
              "r"     '(repeat :wk "Repeat Before Command")
              "s"     '(:wk "Spell Check")
              "d"     '(:wk "Debug")
              "n"     '(:wk "File Manger")
              "f"     '(:wk "Find")
              "g"     '(:wk "Git")
              "o"     '(:wk "Org")
              "p"     '(:wk "Paren")
              "t"     '(:wk "Tabbar")
              "u"     '(:wk "Utils")
              "w"     '(:wk "Windows")
              "h"     '(:wk "Hacking")
              "l"     '(:wk "Lisp or LSP")
              "hr"    '(:wk "Rust")
              "er"    '(restart-emacs :wk "Restart")
              "el"    '(-reload-emacs :wk "Reload")
              "et"    '((lambda ()(interactive) (org-babel-load-file (expand-file-name "config.org" user-emacs-directory))) :wk "tangle config.org" )
              "ot"    '(org-babel-tangle :wk "tangle config.org" )
              "ff"    '(find-file :wk "Find File")
              "fu"    '(browse-url :wk "Browse url")
              "ep"    '(list-processes :wk "Process")
              "ef"    '((lambda ()(interactive)(find-file "~/.emacs.d/config.org")) :wk "configure file")
              "wf"    '(toggle-frame-fullscreen :wk "FullScreen")
              "wh"    '(shrink-window-horizontally :wk "Right size up")
              "wj"    '(enlarge-window :wk "Right size down")
              "wk"    '(shrink-window :wk "Bottom size up")
              "wl"    '(enlarge-window-horizontally :wk "Bootom size down"))
)

(use-package evil-visualstar :ensure t 
; vim visual mode에서 * #를 사용해서 같은 단어 검색가능
:after evil
:config (global-evil-visualstar-mode t)
)

(use-package evil-string-inflection :ensure t
:config (define-key evil-normal-state-map "gR" 'evil-operator-string-inflection)
)

(use-package evil-surround :ensure t 
; @call-function
; visual mode S- or gS-
; normal mode ys- or yS-
; change surround cs-
; delete surround ds-
; @select area
; {call-function}- - ;현재부터 단어 끝까지
; {call-function}-i- ;현재 단어
; {call-function}-s- ;현재 줄
; @wrap function
; {select-area}-w
; ${target}( 바꾸고싶은거 ), ${change}(바뀔거)
; 감싸기:     => y-s-i-w-${change}( "(", "{", "[")
; 전부 감싸기 => y-s-s-${change}
; 바꾸기: => c-s-${target}( "(", "{", "["), ${change}
; 벗기기: => d-s-${target}( "(", "{", "[")
:after  evil
:config (global-evil-surround-mode 1)
)

(use-package evil-exchange :ensure t :disabled
; gx gx (gx로 선택한 영역 교환)
:after evil
:config (evil-exchange-install)
)

(use-package evil-indent-plus :ensure t 
:after evil
:config (evil-indent-plus-default-bindings)
)

(use-package evil-goggles :ensure t :after evil
:config (setq evil-goggles-pulse t)
        (setq evil-goggles-duration 0.500)
        (evil-goggles-mode)
)

(use-package evil-traces :ensure t  :after evil
; move: m +{n}, delete: +{n},+{n}d, join: .,+{n}j glboal: g/{target}/{change}
:config (evil-traces-use-diff-faces)
        (evil-traces-mode)
)

(use-package evil-nerd-commenter :ensure t  :after evil
:general (leader "c" '(:wk "comment")
                 "ci" 'evilnc-comment-or-uncomment-lines
                 "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
                 "cc" 'evilnc-copy-and-comment-lines
                 "cp" 'evilnc-comment-or-uncomment-paragraphs
                 "cr" 'comment-or-uncomment-region
                 "cv" 'evilnc-toggle-invert-comment-line-by-line
                 "\\" 'evilnc-comment-operator)
)

(use-package evil-args :ensure t  :after evil
; change argument: c-i-a, delete arguemnt: d-a-a
:config (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
        (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
        (define-key evil-normal-state-map "L" 'evil-forward-arg)
        (define-key evil-normal-state-map "H" 'evil-backward-arg)
        (define-key evil-motion-state-map "L" 'evil-forward-arg)
        (define-key evil-motion-state-map "H" 'evil-backward-arg)
        (define-key evil-normal-state-map "K" 'evil-jump-out-args)
)


(use-package evil-multiedit :ensure t :after evil)

(use-package evil-matchit :ensure t 
:after evil
:config (global-evil-matchit-mode 1)
)

(use-package evil-lion :ensure t 
; gl ${operator}
:config (evil-lion-mode)
)

(use-package evil-escape :ensure t :disabled
:config (setq-default evil-escape-key-sequence "jk")
)

(use-package evil-numbers :ensure t 
;https://github.com/cofi/evil-numbers
:after evil
:general (leader "="     '(evil-numbers/inc-at-pt :wk "++")
                 "-"     '(evil-numbers/dec-at-pt :wk "--"))
         (nmap   "C-c +" '(evil-numbers/inc-at-pt :wk "++")
                 "C-c -" '(evil-numbers/dec-at-pt :wk "--"))
         (       "C-c +" '(evil-numbers/inc-at-pt :wk "++")
                 "C-c =" '(evil-numbers/inc-at-pt :wk "++")
                 "C-c -" '(evil-numbers/dec-at-pt :wk "--"))
)

(use-package evil-extra-operator :ensure t
:after (evil fold-this)
:config (global-evil-extra-operator-mode 1)
)

(use-package evil-collection :ensure t
:after (evil)
:custom (evil-collection-setup-minibuffer t)
;:init  (add-hook 'magit-mode-hook     (lambda () (evil-collection-magit-setup)     (evil-collection-init)))
;       (add-hook 'neotree-mode-hook   (lambda () (evil-collection-neotree-setup)   (evil-collection-init)))
;       (add-hook 'which-key-mode-hook (lambda () (evil-collection-which-key-setup) (evil-collection-init)))
       ;(add-hook 'evil-mc-mode-hook   (lambda () (evil-collection-evil-mc-setup)   (evil-collection-init)))
:config
       ;(evil-collection-pdf-setup)
       ;(evil-collection-occur-setup)
       ;(evil-collection-buff-menu-setup)
       ;(evil-collection-package-menu-setup)
       ;(evil-collection-eshell-setup)
       ;(evil-collection-calc-setup)
       ;(evil-collection-which-key-setup)
       ;(evil-collection-ivy-setup)
       ;(evil-collection-vterm-setup) 
       ;(evil-collection-wgrep-setup)
       (evil-collection-forge-setup)
       (evil-collection-init)
)



(use-package sudo-mode :no-require t :ensure nil
:preface
(defun sudo-find-file (file-name)
    "sudo open"
    (interactive "FSudo Find File: ")
    (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
        (find-file tramp-file-name)))
:general (leader "fs" #'sudo-find-file)
)

(use-package goto-last-change :ensure t  :defer t
;https://github.com/camdez/goto-last-change.el
:general (leader "fl" 'goto-last-change)
)

(use-package no-littering :ensure t 
:config (require 'recentf)
        (add-to-list 'recentf-exclude no-littering-var-directory)
        (add-to-list 'recentf-exclude no-littering-etc-directory)
        (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
)

(use-package elmacro :ensure t  :disabled :config (elmacro-mode))
; C-x ( 메크로 시작
; C-x ) 메크로 종료
; C-x e 메크로 실행
; C-u 10 C-x e

(use-package beacon :ensure t :config (beacon-mode t))
(use-package git-gutter :ensure t 
:custom
    (git-gutter:lighter       " GG")
    (git-gutter:window-width  1)
    (git-gutter:modified-sign ".")
    (git-gutter:added-sign    "+")
    (git-gutter:deleted-sign  "-")
:config
    (global-git-gutter-mode t)
    (setq-default display-line-numbers-width 3)
    ;(add-hook 'prog-mode-hook 'display-line-numbers-mode)
    ;(add-hook 'org-mode-hook 'display-line-numbers-mode)
    (global-display-line-numbers-mode t)
    (global-hl-line-mode t)
    (set-face-foreground 'git-gutter:added    "#daefa3")
    (set-face-foreground 'git-gutter:deleted  "#FA8072")
    (set-face-foreground 'git-gutter:modified "#b18cce")
)
(use-package highlight-numbers :ensure t
:hook (prog-mode . highlight-numbers-mode)
)

(setq custom-safe-themes t)
(use-package doom-themes :ensure t 
:init    (load-theme   'doom-vibrant t)
         ;(enable-theme 'doom-nord)
:config (doom-themes-org-config)
)
; 자동으로 Dark mode Light mode 변환
(use-package mac-dark-mode :no-require t :disabled
:if *is-mac*
:preface 
(defun set-system-dark-mode ()
    (interactive)
    (if (string= (shell-command-to-string "printf %s \"$( osascript -e \'tell application \"System Events\" to tell appearance preferences to return dark mode\' )\"") "true")
        (load-theme 'doom-one t) ; dark-mode
        (load-theme 'doom-city-lights t)) ; light-mode
)
:config (run-with-idle-timer 60 t (lambda () (set-system-dark-mode)))  ; 1분마다, repeat
)

(use-package all-the-icons :ensure t 
:config  
)
(use-package doom-modeline :ensure t 
:hook   (after-init . doom-modeline-mode)
:init   (setq find-file-visit-truename t)
        (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
        (setq inhibit-compacting-font-caches t)
        (setq doom-modeline-height 30)
        (setq doom-modeline-icon t) ; current version has error
        (setq doom-modeline-persp-name t)
        (setq doom-modeline-major-mode-icon t)
        (setq doom-modeline-enable-word-count t)
        (setq doom-modeline-lsp t)

        (setq doom-modeline-current-window t)
        (setq doom-modeline-env-version t)
        (setq doom-modeline-env-enable-python t)
        ;(setq doom-modeline-python-executable "pipenv")
        (setq doom-modeline-env-enable-ruby t)
        (setq doom-modeline-env-ruby-executable "ruby")
        (setq doom-modeline-env-enable-elixir t)
        (setq doom-modeline-env-elixir-executable "iex")
        (setq doom-modeline-env-enable-go t)
        (setq doom-modeline-env-go-executable "go")
        (setq doom-modeline-env-enable-perl t)
        (setq doom-modeline-env-perl-executable "perl")
        (setq doom-modeline-env-enable-rust t)
        (setq doom-modeline-env-rust-executable "rustc")
        (setq doom-modeline-github t)
        ;(setq doom-modeline-iconer-state-icon t)
        ;(setq doom-modeline--battery-status t)
        (setq doom-modeline--flycheck-icon t)
        (setq doom-modeline-current-window t)
        (setq doom-modeline-major-mode-color-icon t)
:config (add-hook 'after-init-hook 'doom-modeline-mode)
        (with-eval-after-load 'lsp-treemacs (doom-themes-treemacs-config))
)

(use-package hide-mode-line :ensure t 
:after (neotree)
:hook  (neotree-mode . hide-mode-line-mode)
)

(use-package nyan-mode :ensure t 
;:after  (doom-modeline)
:config (setq nyan-wavy-trail t)
        (nyan-mode)
        (nyan-start-animation)
)

(use-package fancy-battery :ensure t 
:hook   (after-init . fancy-battery-mode)
:config (fancy-battery-default-mode-line)
        (setq fancy-battery-show-percentage t)
)

(use-package diminish :ensure t  :defer t
:init
    (diminish 'c++-mode "C++ Mode")
    (diminish 'c-mode   "C Mode"  )
)

;(load-library "hideshow")
;    (global-set-key (kbd "<C-l>") 'hs-show-block)
;    (global-set-key (kbd "<C-h>") 'hs-hide-block)
;    (add-hook 'c-mode-common-hook     'hs-minor-mode)
;    (add-hook 'emacs-lisp-mode-hook   'hs-minor-mode)
;    (add-hook 'java-mode-hook         'hs-minor-mode)
;    (add-hook 'lisp-mode-hook         'hs-minor-mode)
;    (add-hook 'perl-mode-hook         'hs-minor-mode)
;    (add-hook 'sh-mode-hook           'hs-minor-mode)

(use-package aggressive-indent :ensure t  :disabled
; https://github.com/Malabarba/aggressive-indent-mode
:config (electric-indent-mode nil)
;exclud mode
;(add-to-list 'aggresive-indent-excluded-modes 'html-mode)
)

(use-package highlight-indentation :ensure t
:hook (prog-mode . highlight-indentation-mode)
)

(use-package indent4-mode :no-require t 
:preface
    (defun my-set-indent (n)
        (setq-default tab-width n)
        ;(electric-indent-mode n)
        (setq-default c-basic-offset n)
        (setq lisp-indent-offset n)
        (setq indent-line-function 'insert-tab)
    )
    (defun un-indent-by-removing-4-spaces ()
        "back tab"
        (interactive)
        (save-excursion
        (save-match-data
        (beginning-of-line)
        ;; get rid of tabs at beginning of line
        (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
            (when (looking-at "^    ")
                (replace-match "")))
            )
    )
:config
    (global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)
    (electric-indent-mode nil)
    (my-set-indent 4)
    (setq-default indent-tabs-mode nil)
)

(use-package paren :ensure t 
:init   (show-paren-mode 0)
        (electric-pair-mode 0)
:config (setq show-paren-delay 0)
)

(use-package expand-region :ensure t 
:general (leader "tw" '(er/expand-region :wk "Text Wrap"))
)

(use-package rainbow-delimiters :ensure t 
:hook ((prog-mode text-mode) . rainbow-delimiters-mode) 
)

(use-package smartparens :ensure t 
;:general (leader "pr " 'sp-rewrap-sexp
;                 "pll" 'sp-forward-slurp-sexp
;                 "phh" 'sp-backward-slurp-sexp
;                 "plh" 'sp-forward-barf-sexp
;                 "phl" 'sp-backward-barf-sexp)
:init (smartparens-global-mode)
      (show-smartparens-global-mode)
)
;elisp double quote problem fix setting
(use-package smartparens-config :ensure smartparens)

(use-package hydra :ensure t  :defer t)

(use-package which-key :ensure t 
:init   (which-key-mode t)
:config (setq which-key-allow-evil-operators t)
        (setq which-key-show-operator-state-maps t)
        ;(which-key-setup-minibuffer)
)
(use-package which-key-posframe :ensure t  :disabled
:after which-key
:config
    (setq which-key-posframe-border-width 15)
    (setq which-key-posframe-poshandler 'posframe-poshandler-window-top-center)
    (which-key-posframe-mode)
)

(use-package avy :ensure t 
:general (leader "jl" '(avy-goto-line :wk "Jump to line")
                 "jw" '(avy-goto-char :wk "Jump to word"))
)

(use-package prescient :ensure t :disabled)

(use-package ivy :ensure t 
;:after evil-collection
 ;ivy S-SPC remapping toogle-input-method
:general ("M-x" 'counsel-M-x )
         (:keymaps 'ivy-minibuffer-map
                        "S-SPC" 'toggle-input-method
                        "<f17>" 'toggle-input-method)
:custom (ivy-use-virtual-buffers      t)
        (ivy-use-selectable-prompt    t)
        (enable-recursive-minibuffers t)
        (ivy-height 20)
        (ivy-count-format "(%d/%d) ")
        (ivy-display-style 'fancy)
        (ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy) (t . ivy--regex-plus)))
        (ivy-format-function 'ivy-format-function-line)
:config 
        (setq ivy-initial-inputs-alist nil)
        ;(setq search-default-mode #'char-fold-to-regexp)
        (ivy-mode 1)
)

(use-package counsel
:after ivy
:config (counsel-mode)
)

(use-package swiper :ensure t 
:after ivy
:general ("C-s"    'swiper)
         ("C-S-s"  'swiper-all)
:config (setq swiper-action-recenter t)
        (setq swiper-goto-start-of-match t)
        (setq swiper-stay-on-quit t)
)

(use-package ivy-posframe :ensure t 
:after ivy
:custom (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
        (ivy-posframe-parameters '((left-fringe . 8) (right-fringe . 8) (internal-border-width . 10)))
         ;ivy-posframe mutli frame focus bug fix
        ;(ivy-posframe-width 120)
:config ;(setq ivy-posframe-height-alist '((t . 20)))
        (add-function :after after-focus-change-function (lambda () (posframe-delete-all)))
        (setq ivy-posframe-height-fixed t)
        (setq ivy-posframe-widtivy-posframe-height-fixedh-fixed t)
        (ivy-posframe-mode t)
)

(use-package counsel-osx-app :ensure t 
:after counsel
:general (leader "fa" '(counsel-osx-app :wk "Execute OSX App"))
)

(use-package counsel-fd :ensure t  :disabled
:after counsel
:commands (counsel-fd-dired-jump counsel-fd-file-jump)
)


(use-package ivy-yasnippet :ensure t 
:after (ivy yasnippet)
:general  ("C-c C-y" 'ivy-yasnippet)
;:config (advice-add #'ivy-yasnippet--preview :override #'ignore)
)

(use-package historian :ensure t
:after  (ivy)
:config (historian-mode)
)

(use-package ivy-historian :ensure t 
:after  (ivy historian)
:config (ivy-historian-mode)
)

(use-package all-the-icons-ivy :ensure t 
:config (all-the-icons-ivy-setup)
)

(use-package ivy-xref :ensure t  :disabled
:after (ivy xref)
:config (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
)

(use-package lsp-ivy :ensure t 
:general (leader "hs" '(lsp-ivy-workspace-symbol :wk "Search Symbol")
                 "hS" '(lsp-ivy-global-workspace-symbol :wk "Search Global Symbol"))
)

(use-package counsel-projectile :ensure t 
:after  (counsel projectile)
:custom (projectile-completion-system 'ivy)
        (counsel-find-file-ignore-regexp ".ccls-cache/")
:general (leader "fp" '(counsel-projectile-find-file-dwim   :wk "Search in Project")
                 "fG" '(counsel-projectile-rg               :wk "Grep in Project")
                 "bS" '(counsel-project
ile-switch-to-buffer :wk "Search Buffer in Project"))
          
:config (counsel-projectile-mode 1)

)
(use-package counsel-world-clock :ensure t 
:after (counsel)
:general (:keymaps 'counsel-mode-map "C-c c k"  'counsel-world-clock)
)

(use-package counsel-tramp :ensure t 
:after counsel
:commands counsel-tramp
:general ("C-c s" 'counsel-tramp)
:init (setq tramp-default-method "ssh")
)

(use-package counsel-org-clock :ensure t  :after (counsel org))

(use-package all-the-icons-ivy-rich :ensure t 
:config
    (setq ivy-rich-parse-remote-buffer nil)
    (all-the-icons-ivy-rich-mode t)
)

(use-package ivy-rich :ensure t 
:init (setq ivy-rich-path-style    'abbrev)
      (setq ivy-virtual-abbreviate 'full)
:config (ivy-rich-mode 1)
)

(use-package smex :ensure t 
:general (leader "fm" #'smex-major-mode-commands)
:init (smex-initialize)
)

(use-package projectile :ensure t 
:after ivy
:init   (projectile-mode t)
:config (setq projectile-require-project-root nil)
        (setq projectile-enable-caching t)
        (setq projectile-globally-ignored-directories
            (append '(".ccls-cache" ".git" "__pycache__") projectile-globally-ignored-directories))
        (setq projectile-completion-system 'ivy)
        (setq projectile-current-project-on-switch t)
        (evil-ex-define-cmd "kp" 'projectile-kill-buffers)
        ;(setq projectile-project-root-files-functions #'(projectile-root-top-down
        ;                                                 projectile-root-top-down-recurring
        ;                                                 projectile-root-bottom-up
        ;                                                 projectile-root-local))
        ;(setq projectile-globally-ignored-files
        ;    (append '() projectile-globaly-ignore-files))
)

(use-package neotree :ensure t 
:after (projectile all-the-icons)
:commands (neotree-toggle)
:general (leader "n" #'neotree-toggle)
:init
    (setq projectile-switch-project-action 'neotree-projectile-action)
    (setq-default neo-smart-open t)
:config
    (setq-default neo-window-width 30)
    (setq-default neo-dont-be-alone t)
    (add-hook 'neotree-mode-hook (lambda () (display-line-numbers-mode -1) ))
    (setq neo-force-change-root t)
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
    (setq neo-show-hidden-files t)
)
(use-package all-the-icons-dired :ensure t 
:after all-the-icons
:init  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(defun copy-file-name-to-clipboard ()
    "Copy the current buffer file name to the clipboard."
    (interactive)
    (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name))))
        (when filename
        (kill-new filename)
            (message "Copied buffer file name '%s' to the clipboard." filename)))
)

(use-package ace-window :ensure t 
:commands (ace-window)
:general (leader "wo" 'ace-window
                 "wd" 'delete-other-windows)
         ;("C-w C-o" 'ace-window)
:config (setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8))
)

(use-package eyebrowse :ensure t :defer t
:init (eyebrowse-mode t)
:general (leader "w;" 'eyebrowse-last-window-config
                 "w0" 'eyebrowse-close-window-config
                 "w1" 'eyebrowse-switch-to-window-config-1
                 "w2" 'eyebrowse-switch-to-window-config-2
                 "w3" 'eyebrowse-switch-to-window-config-3
                 "w4" 'eyebrowse-switch-to-window-config-4
                 "w5" 'eyebrowse-switch-to-window-config-5
                 "w6" 'eyebrowse-switch-to-window-config-6
                 "w7" 'eyebrowse-switch-to-window-config-7)
)

(use-package window-purpose :ensure t  :disabled)

(use-package magit :ensure t :pin melpa
:commands magit-status
:general (leader "gs" 'magit-status)
:config (setq vc-handled-backends nil)
        ;(setq auth-source '("~/.authinfo"))
)

(use-package forge :ensure t  :after magit
    :config
    ;(defclass forge-gitlab-http-repository (forge-gitlab-repository)
    ;    ((issues-url-format         :initform "http://%h/%o/%n/issues")
    ;     (issue-url-format          :initform "http://%h/%o/%n/issues/%i")
    ;     (issue-post-url-format     :initform "http://%h/%o/%n/issues/%i#note_%I")
    ;     (pullreqs-url-format       :initform "http://%h/%o/%n/merge_requests")
    ;     (pullreq-url-format        :initform "http://%h/%o/%n/merge_requests/%i")
    ;     (pullreq-post-url-format   :initform "http://%h/%o/%n/merge_requests/%i#note_%I")
    ;     (commit-url-format         :initform "http://%h/%o/%n/commit/%r")
    ;     (branch-url-format         :initform "http://%h/%o/%n/commits/%r")
    ;     (remote-url-format         :initform "http://%h/%o/%n")
    ;     (create-issue-url-format   :initform "http://%h/%o/%n/issues/new")
    ;     (create-pullreq-url-format :initform "http://%h/%o/%n/merge_requests/new")
    ;     (pullreq-refspec :initform "+refs/merge-requests/*/head:refs/pullreqs/*")))
    ;(add-to-list 'ghub-insecure-hosts "git.private.network.repo/api/v4")
)

(use-package git-messenger :ensure t
:commands git-messenger:popup-message
:general (leader "gm" 'git-messenger:popup-message)
:config (setq git-messenger:use-magit-popup t)
)


; 현재 git repo의 homepage link를 clipboard에 넣어준다
(use-package git-link :ensure t
:general (leader "gh" 'git-link-homepage)
:config  ;(setq git-link-use-single-line-number t)
         (setf git-link-use-commit t)
)

;; git history view mode
(use-package smeargle :ensure t 
:commands smeagle
)

(use-package blamer :ensure t :defer t
:custom
    (blamer-view 'overlay)
    (blamer-idle-time 0.3)
    (blamer-min-offset 70)
    (blamer-force-truncate-long-line t)
:custom-face
    (blamer-face ((t :foreground "#7a88cf"
                     :background nil
                     :height 1.0
                     :italic t)))
)

(use-package evil-ediff :ensure t 
:after evil
:config (evil-ediff-init)
)

(use-package undo-tree :ensure t  :diminish undo-tree-mode
:commands (undo-tree-undo undo-tree-redo)
:general (leader "uu" 'undo-tree-undo
                 "ur" 'undo-tree-redo)
:init
    (evil-define-key 'normal 'global (kbd "C-r") #'undo-tree-redo)
    (evil-define-key 'normal 'global "u" #'undo-tree-undo)
    (defalias 'redo 'undo-tree-redo)
    (defalias 'undo 'undo-tree-undo)
:config
    (global-undo-tree-mode)
)

(use-package org
:general (leader "oa" 'org-agenda
                 "ob" 'org-iswitchb
                 "oc" 'org-capture
                 "oe" 'org-edit-src-code
                 "ok" 'org-edit-src-exit
                 "ol" 'org-store-link)
;:init   (setq org-directory          (expand-file-name     "~/Dropbox/org   "))
;        (setq org-default-notes-file (concat org-directory "/notes/notes.org"))
:config (setq org-startup-indented   nil)
)

(use-package org-superstar :ensure t 
:after org
:hook (org-mode . org-superstar-mode)
:custom (org-superstar-special-todo-items t)
;:custom-face 
;    (org-level-1 ((t (:inherit outline-1 :height 1.3))))
;    (org-level-2 ((t (:inherit outline-2 :height 1.2))))
;    (org-level-3 ((t (:inherit outline-3 :height 1.1))))
;    (org-level-4 ((t (:inherit outline-4 :height 1.0))))
;    (org-level-5 ((t (:inherit outline-5 :height 1.0))))
)

(use-package org-journal :ensure t :disabled
:after org
:preface
    (defun org-journal-find-location ()
        (org-journal-new-entry t)
        (goto-char (point-min)))
:config
    (setq org-journal-dir (expand-file-name "~/Dropbox/org/journal")
            org-journal-file-format "%Y-%m-%d.org"
            org-journal-date-format "%Y-%m-%d (%A)")
    (add-to-list 'org-agenda-files (expand-file-name "~/Dropbox/org/journal"))
    (setq org-journal-enable-agenda-integration t
            org-icalendar-store-UID t
            org-icalendar-include0tidi "all"
            org-icalendar-conbined-agenda-file "~/calendar/org-journal.ics")
    (org-journal-update-org-agenda-files)
    (org-icalendar-combine-agenda-files)
)

(use-package org-capture :ensure nil :disabled
:after org
:config (setq org-reverse-note-order t)
    (add-to-list 'org-agenda-files (expand-file-name "~/Dropbox/org/notes"))
    (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/Dropbox/org/notes/notes.org" "Todos")
            "* TODO %?\nAdded: %U\n" :prepend t :kill-buffer t)
            ("l" "Link" entry (file+headline "~/Dropbox/org/notes/notes.org" "Links")
            "* TODO %?\nAdded: %U\n" :prepend t :kill-buffer t)
            ("j" "Journal" entry (function org-journal-find-location)
            "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
            ("a" "Appointment" entry (file "~/Dropbox/org/agenda/gcal.org")
            "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
            )
    )
)

(use-package org-agenda :ensure nil :disabled
:after org
:config (use-package evil-org :ensure t 
        :after (org evil)
        :init (add-hook 'org-mode-hook 'evil-org-mode)
            (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme)))
            (setq org-agenda-files '("~/.emacs.d/private/schedule.org"))
            (require 'evil-org-agenda)
            (evil-org-agenda-set-keys)
        )
)

(use-package org-pomodoro :ensure t 
:after org-agenda
:custom
    (org-pomodoro-ask-upon-killing t)
    (org-pomodoro-format "%s")
    (org-pomodoro-short-break-format "%s")
    (org-pomodoro-long-break-format  "%s")
:custom-face
    (org-pomodoro-mode-line         ((t (:foreground "#ff5555"))))
    (org-pomodoro-mode-line-break   ((t (:foreground "#50fa7b"))))
:hook
    (org-pomodoro-started  . (lambda () (notifications-notify
        :title "org-pomodoro"
        :body "Let's focus for 25 minutes!"
        :app-icon "~/.emacs.d/img/001-food-and-restaurant.png")))
    (org-pomodoro-finished . (lambda () (notifications-notify
        :title "org-pomodoro"
        :body "Well done! Take a break."
        :app-icon "~/.emacs.d/img/004-beer.png")))
:general (:keymaps 'org-agenda-mode-map "p"  'org-pomodoro)
)

(use-package org-table-auto-align-mode :load-path "lisp/org-table-auto-align-mode.el" :ensure nil :disabled
:after org
:hook (org-mode . org-table-auto-align-mode)
)

(use-package orgtbl-aggregate :ensure t  :defer t)

(use-package toc-org :ensure t  :after org
:hook (org-mode . toc-org-mode)
;:config (add-hook 'org-mode-hook 'toc-org-mode)
)


(use-package calfw :ensure t :disabled
:commands cfw:open-calendar-buffer
:config (use-package calfw-org :config (setq cfw:org-agenda-schedule-args '(:deadline :timestamp :sexp)))
)

(use-package calfw-gcal :ensure t  :disabled
:init (require 'calfw-gcal))

(use-package ob-restclient :ensure t 
:after  (org restclient)
:config (org-babel-do-load-languages 'org-babel-load-languages '((restclient . t)))
)

(use-package org-babel :no-require t
:after org
:config (org-babel-do-load-languages
            'org-babel-load-languages
            '((emacs-lisp . t)
              (python     . t)
              (org        . t)
              (shell      . t)
              (C          . t)))
)

(use-package olivetti :ensure t 
:commands (olivetti-mode)
:config (setq olivetti-body-width 120))

(use-package typo :ensure t :commands (type-mode))

(use-package poet-theme :ensure t  :defer t)

(use-package writeroom-mode :ensure t 
:commands (writeroom-mode)
:config (setq writeroom-width 100)
)

(define-minor-mode writer-mode
    "poet use writer mode"
    :lighter " writer"
    (if writer-mode
        (progn
            ;(olivetti-mode 1)
            ;(typo-mode 1)
            (beacon-mode 0)
            (display-line-numbers-mode -1)
            (git-gutter-mode 0)
            (writeroom-mode 1))
        ;(olivetti-mode 0)
        ;(typo-mode 0)
        (beacon-mode 1)
        (display-line-numbers-mode 1)
        (git-gutter-mode 1)
        (writeroom-mode 0)))

(use-package mu4e :ensure t  :disabled :commands (mu4e))

(use-package rainbow-mode :ensure t 
:hook   (prog-mode text-mode)
:config (rainbow-mode)
)

(use-package docker :ensure t  
:commands docker
:general (leader "hud" 'docker)
:custom (docker-image-run-arguments '("-i", "-t", "--rm"))
)

(use-package dockerfile-mode :ensure t 
:mode ("Dockerfile\\'" . dockerfile-mode)
)

(use-package kubernetes :ensure t :commands (kubernetes-overview))

;; If you want to pull in the Evil compatibility package.
(use-package kubernetes-evil :ensure t :after kubernetes)

(use-package k8s-mode :ensure t
:hook (k8s-mode . yas-minor-mode)
)

(use-package docker-compose-mode :ensure t)

(use-package exec-path-from-shell :ensure t :defer 1
;:if     (memq window-system '(mac ns x))
:config (exec-path-from-shell-initialize)
        (exec-path-from-shell-copy-env "PATH")
)
  
(use-package vterm :ensure t :after (evil-collection exec-path-from-shell)
:commands (vterm)
;(zsh . "chsh -s $(which zsh)")
;:ensure-system-package ((zsh))
                        ;(zinit . "sh -c \"$(curl -fsSL https://git.io/zinit-install)\""))
;:init   (setq vterm-always-compile-module t)
:config 
        (define-key vterm-mode-map (kbd "C-c C-c") 'vterm-send-C-c)
        (define-key vterm-mode-map (kbd "<C-return>") 'vterm-send-right)
        (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))
        (add-hook 'vterm-mode-hook #'evil-collection-vterm-escape-stay)
)


(use-package multi-vterm :ensure t 
:general (leader "tn" 'multi-vterm :wk "new terminal")
)

(use-package vterm-with-centaur-tab :no-require t
:after (vterm-toggle centaur-tabs)
:preface (defun vmacs-awesome-tab-buffer-groups ()
          "`vmacs-awesome-tab-buffer-groups' control buffers' group rules. "
          (list
           (cond
            ((derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode) "Term")
            ((string-match-p (rx (or "\*Helm"
                                     "\*helm"
                                     "\*tramp"
                                     "\*Completions\*"
                                     "\*sdcv\*"
                                     "\*Messages\*"
                                     "\*Ido Completions\*"))
                                     (buffer-name))
             "Emacs")
            (t "Common"))))
        (defun vmacs-term-mode-p(&optional args)
            (derived-mode-p 'eshell-mode 'term-mode 'shell-mode 'vterm-mode))
:config (setq centaur-tabs-buffer-groups-function   'vmacs-awesome-tab-buffer-groups)
        (setq vterm-toggle--vterm-buffer-p-function 'vmacs-term-mode-p)
)

(use-package shell-pop :ensure t
:custom (shell-pop-shell-type '("term" "vterm" (lambda () (vterm) )))
        (shell-pop-term-shell "/bin/zsh")
        (shell-pop-full-span t)
:general (leader "ut"'shell-pop)
:init    (global-set-key (kbd "<C-t>") 'shell-pop)
)

(use-package with-editor :ensure t :disabled
:hook ((shell-mode term-exec eshll-mode vterm-mode) . with-editor-export-editor)
)

(use-package vterm-command :no-require t
:after (vterm)
:preface
(defun run-in-vterm-kill (process event)
  "A process sentinel. Kills PROCESS's buffer if it is live."
  (let ((b (process-buffer process)))
    (and (buffer-live-p b)
         (kill-buffer b))))

(defun run-in-vterm (command)
  "Execute string COMMAND in a new vterm.
Interactively, prompt for COMMAND with the current buffer's file
name supplied. When called from Dired, supply the name of the file at point.
Like `async-shell-command`, but run in a vterm for full terminal features.
The new vterm buffer is named in the form `*foo bar.baz*`, the
command and its arguments in earmuffs.
When the command terminates, the shell remains open, but when the
shell exits, the buffer is killed."
  (interactive
   (list
    (let* ((f (cond (buffer-file-name)
                    ((eq major-mode 'dired-mode)
                     (dired-get-filename nil t))))
           (filename (concat " " (shell-quote-argument (and f (file-relative-name f))))))
      (read-shell-command "Terminal command: "
                          (cons filename 0)
                          (cons 'shell-command-history 1)
                          (list filename)))))
  (with-current-buffer (vterm (concat "*" command "*"))
    (set-process-sentinel vterm--process #'run-in-vterm-kill)
    (vterm-send-string command)
    (vterm-send-return)))
)

(use-package eshell :disabled
:commands eshell
:config (setq eshell-buffer-maximum-lines 1000)
        ;(require 'xterm-color)
        (add-hook 'eshell-mode-hook (lambda () (setq pcomplete-cycle-completions     nil)))
        ;(add-hook 'eshell-mode-hook (lambda () (setq xterm-color-preserve-properties t) (setenv "TERM" "xterm-256color")))
        (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
        (setq eshell-output-filter-functions (remove 'eshell-handle-asni-color eshell-output-filter-functions))
        (setq eshell-cmpl-cycle-completions nil)
)

(use-package eshell-did-you-mean :ensure t 
:after  eshell
:config (eshell-did-you-mean-setup)
)

(use-package esh-help :ensure t 
:after (eshell eldoc)
:config (setup-esh-help-eldoc)
)

(use-package eshell-prompt-extras :ensure t 
:after eshell
:config
    (autoload 'epe-theme-lambda   "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil)
    (setq eshell-prompt-function  'epe-theme-lambda)
)

(use-package fish-completion :ensure t 
:after eshell
:config (when (and (executable-find "fish")
                   (require 'fish-completion nil t))
              (global-fish-completion-mode))
)

(use-package esh-autosuggest :ensure t 
:after eshell
:hook (eshell-mode . esh-autosuggest-mode)
)

(use-package eshell-up :ensure t :disabled
:after eshell
:config (add-hook 'eshell-mode-hook (lambda () (eshell/alias "up" "eshell-up $1")
                                          (eshell/alias "pk" "eshell-up-peek $1")))
)

(use-package command-log-mode :ensure t  :defer t)

(use-package emojify :ensure t 
:if window-system
:config 
        (setq emojify-display-style 'image)
        ;(setq emojify-emoji-styles  '(unicode))
        ;(setq emojify-emoji-set "emojione-v2.2.6")
        (global-emojify-mode 1)
)

(use-package buffer-move :ensure t  :defer t
:general (leader "b c" #'clean-buffer-list
                 "b s" 'switch-to-buffer
                 "b r" 'eval-buffer
                 "b h" 'buf-move-left
                 "b j" 'buf-move-down
                 "b k" 'buf-move-up
                 "b l" 'buf-move-right
                 "b m" 'switch-to-buffer
                 "b n" 'next-buffer
                 "b p" 'previous-buffer)
:init  (global-set-key (kbd "C-x C-b") 'switch-to-buffer)
)

(use-package all-the-icons-ibuffer :ensure t 
:after all-the-icons
:hook (ibuffer-mode . all-the-icons-ibuffer-mode)
)

(use-package org-roam :ensure t :disabled
:custom  (org-roam-dailies-directory "journals/")
:general (leader "of" '(org-roam-node-find :wk "Note"))
:custom  (org-roam-directory (expand-file-name "~/GDrive/Roam/"))
:config
    (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?"
            :if-new (file+head "%<%Y-%m-%d>.org"
                               "#+title: %<%Y-%m-%d>\n"))))
    (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
    (org-roam-db-autosync-enable)
    (require 'org-roam-protocol) ;; If using org-roam-protocol
    ;(org-roam-setup)
)

(use-package websocket :ensure t :after org-roam)

(use-package org-roam-ui :ensure t
:after org-roam
:config (setq org-roam-ui-sync-theme t)
        (setq org-roam-ui-follow t)
        (setq org-roam-ui-update-on-save t)
        (setq org-roam-ui-open-on-start t)
)

(use-package buffer-zoom :no-require t 
:general (leader "tu" 'text-scale-increase
                 "td" 'text-scale-decrease)
)
;
;(use-package org-roam-server :ensure t  :after (org-roam)
;:commands org-roam-server-mode
;:config
;    (setq org-roam-server-host "127.0.0.1"
;          org-roam-server-port 8080
;          org-roam-server-export-inline-images t
;          org-roam-server-authenticate nil
;          org-roam-server-network-poll t
;          org-roam-server-network-arrows nil
;          org-roam-server-network-label-truncate t
;          org-roam-server-network-label-truncate-length 60
;          org-roam-server-network-label-wrap-length 20)
;)

(use-package dash :ensure t  :defer t
:init (global-dash-fontify-mode t)
)
(use-package dash-functional :ensure t :after dash)

(use-package ialign :ensure t  :defer t
:general (leader "ta" 'ialign))

(use-package page-break-lines :ensure t  :defer t)
(use-package dashboard :ensure t 
:init (dashboard-setup-startup-hook)
:config
    (add-hook 'dashboard-mode-hook (lambda () (display-line-numbers-mode -1) ))
    (setq dashboard-banner-logo-title "Happy Hacking")
    (setq dashboard-startup-banner "~/.emacs.d/image/emacs_icon.png") ;banner image change
    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)
    (setq dashboard-show-shortcuts nil)
    (setq dashboard-set-navigator t)
    (setq dashboard-week-agenda t)
    (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
    ;(setq dashboard-center-content t)
    (setq dashboard-set-init-info t)
    (setq dashboard-items '((recents   . 5)
                            (bookmarks . 5)
                            (projects  . 5)
                            (agenda    . 5)))
)

(use-package centaur-tabs :ensure t 
:general (leader "th" 'centaur-tabs-backward
                 "tl" 'centaur-tabs-forward)
:hook   (dashboard-mode . centaur-tabs-local-mode)
        (vterm-mode     . centaur-tabs-local-mode)
:custom (centaur-tabs-background-color (face-background 'default))
        (centaur-tabs-set-icons t)
        (centaur-tabs-set-bar 'over)
        (centaur-tabs-set-close-button t)
        (centaure-tabs-set-bar t)
        (centaur-tabs-style "chamfer")
:config (setq centaur-tabs-height 26)
        (setq centaur-tabs-cycle-scope 'tabs)
        (centaur-tabs-mode t)
        (centaur-tabs-headline-match)
        (centaur-tabs-group-by-projectile-project)
        (defun centaur-tabs-hide-tab (x)
            "Do no to show buffer X in tabs."
            (let ((name (format "%s" x)))
                (or ;; Current window is not dedicated window.
                    (window-dedicated-p (selected-window))
                    ;; Buffer name not match below blacklist.
                    (string-prefix-p "*epc" name)
                    (string-prefix-p "*helm" name)
                    (string-prefix-p "*Helm" name)
                    (string-prefix-p "*Compile-Log*" name)
                    (string-prefix-p "*lsp" name)
                    (string-prefix-p "*company" name)
                    (string-prefix-p "*Flycheck" name)
                    (string-prefix-p "*tramp" name)
                    (string-prefix-p " *Mini" name)
                    (string-prefix-p "*help" name)
                    (string-prefix-p "*straight" name)
                    (string-prefix-p "*temp" name)
                    (string-prefix-p "*Help" name)
                    (string-prefix-p "*pyright*" name)
                    (string-prefix-p "*pyright::stderr*" name)
                    (string-prefix-p "*Async-native-compile-log*" name)
                    (string-prefix-p "config.org[" name)
                    ;; Is not magit buffer.
                    (and (string-prefix-p "magit" name)
                        (not (file-name-extension name)))
                    )))
)

(use-package symon :ensure t  :defer t)

(use-package google-this :ensure t 
:commands google-this
:general (leader "fw" '(google-this :wk "Search Word"))
:config  (google-this-mode 1)
)

;; google translation
(use-package go-translate :ensure t
:general (leader "ft" 'gts-do-translate)
:config
    (setq gts-translate-list '(("en" "ko") ("ko" "en") ("jp" "ko") ("ko" "jp")))
    (setq gts-default-translator
        (gts-translator
            :picker (gts-prompt-picker)
            :engines (list (gts-bing-engine) (gts-google-engine))
            :render (gts-buffer-render)))
)

(use-package flyspell :ensure t :after flycheck
:hook ((prog-mode . flyspell-prog-mode)
       (text-mode . flyspell-mode))
:general (leader "sk" '((lambda () (interactive) (ispell-change-dictionary "ko_KR") (flyspell-buffer)) :wk "Spell Dictionary Korean")
                 "se" '((lambda () (interactive) (ispell-change-dictionary "en_US") (flyspell-buffer)) :wk "Spell Dictionary English"))
:custom (ispell-dictionary   "en_US")
        (ispell-program-name "aspell")
:config
    ;; 스펠체크 넘어가는 부분 설정
    (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
    (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
    (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))
)

(use-package flyspell-correct-ivy :ensure t  
:after (ivy flyspell)
:general  (:keymaps 'flyspell-mode-map "C-c $" 'flyspell-correct-word-generic)
          (:keymaps 'flyspell-mode-map [remap flyspell-correct-word-before-point]  'flyspell-correct-previous-word-generic)
          (leader "ss" '(flyspell-correct-wrapper :wk "Suggestion"))
)

(use-package wgrep :ensure t 
:after evil-collection
:config (setq wgrep-auto-save-buffer t)
        (evil-collection-wgrep-setup)
       ;(setq wgrep-enable-key "r")
)

(use-package iedit :ensure t 
:general (leader "ie" 'iedit-mode)
)

; package testing 
(use-package try :ensure t  :defer t)

(use-package org-use-package :no-require t :ensure nil
:after (evil org)
:preface
(defun org-use-package-install ()
    "org babel emacs config evaluate"
    (interactive)
    (org-babel-tangle)
    (org-babel-execute-maybe)
    (undo-tree-undo))
:general (leader "oi" 'org-use-package-install
                 ;"ot" 'polymode-next-chunk
                 "oh" 'polymode-previous-chunk
                 "or" 'save-buffer)
)

(use-package helm :disabled
    :config (load-file "~/.emacs.d/lisp/helm-mode.el")
)

(use-package pdf-tools :ensure t  :defer t)

(use-package smeargle :ensure t )

(use-package polymode :ensure t 
:hook (polymode . centaur-tabs-mode-hook) 
:init (add-hook 'polymode-init-inner-hook #'evil-normalize-keymaps)
:custom (polymode-display-process-buffers nil)
)

(use-package poly-jetbrain-lua :no-require t :after polymode
:config
    ; jetbrain golang lua mode
    (define-hostmode poly-golang-lua-hostmode :mode 'go-mode)
    (define-innermode poly-golang-lua-innermode
        :mode 'lua-mode
        :head-matcher "// language=lua\n.*`$"
        :tail-matcher "^`$"
        ;:mode-matcher (cons "")
        :head-mode 'host
        :tail-mode 'host
        )
    (define-polymode poly-golang-lua-mode
        :hostmode   'poly-golang-lua-hostmode
        :innermodes '(poly-golang-lua-innermode))
    ; --
)



(use-package poly-org :ensure t
:hook (org-mode . poly-org-mode)
      (poly-org-mode . git-gutter-mode)
:init (evil-set-initial-state 'poly-org-mode 'normal)
)

(use-package tldr :ensure t 
:commands tldr
:custom (tldr-enabled-categories '("common" "linux" "osx" "sunos"))
)

; FiraCode같은 텍스트모드 활성 모드
(use-package ligature :load-path "lisp/ligature"
;:ensure (:host github :repo "mickeynp/ligature.el")
:config
; Enable the www ligature in every possible major mode
(ligature-set-ligatures 't '("www"))
(ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
; Enable ligatures in programming modes                                                           
(ligature-set-ligatures '(prog-mode org-mode)
        '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
          ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
          "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
          "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
          "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
          "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
          "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
          "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
          "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
          "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
(global-ligature-mode t)
)

(use-package ssh-config-mode :ensure t
:config (add-to-list 'auto-mode-alist '("/\\.ssh/config\\'" . ssh-config-mode))
)

(use-package ssh-deploy :ensure t
:hook ((after-save . ssh-deploy-after-save)
       (find-file . ssh-deploy-find-file))
:config (ssh-deploy-line-mode)
        (ssh-deploy-add-menu)
)

(use-package smudge :ensure t :defer t
; in private/token.el
:general (leader "sn" 'smudge-controller-next-track
                 "hp" 'smudge-controller-previous-track)
:config  (setq smudge-transport 'connect)
)

; slack config in private token setting
(use-package alert
:commands (alert)
:init (setq alert-default-style 'notifier))

; 오직 company-complete-selection으로 만 해야지 snippet 자동완성이 작동됨
(use-package company :ensure t 
:init (global-company-mode 1)
:config
    (company-tng-mode t)
    (setq company-show-quick-access t)
    (setq company-idle-delay 0)
    (setq company--transform-candidates nil)
    (setq company-minimum-prefix-length 1)
    (setq company-tooltip-align-annotations nil)
    (setq company-dabbrev-downcase nil)
    ;(add-to-list 'company-backends #'company-capf)
    ;(add-to-list 'company-backends '(company-capf :with company-yasnippet))
)


(use-package company-statistics :ensure t 
:after company
:config (company-statistics-mode)
)

;company-quickhelp speed up setting
(use-package company-posframe :ensure t 
:after company
:config (company-posframe-mode 1)
)

(use-package company-suggest :ensure t
:config (setq company-suggest-complete-sentence t)
        (add-to-list 'company-backend 'company-suggest-google)
)


(use-package company-box :ensure t :diminish ""
:after company-mode
:hook   (company-mode . company-box-mode)
:custom (company-box-max-candidates 30)
:config (setq company-box-icons-unknown 'fa_question_circle)
        (setq company-box-color-icon t)
        (setq company-box-backends-colors nil)
        (setq company-box-icons-yasnippet 'fa_bookmark)
        (setq company-box-icons-lsp
            '((1  . fa_text_height) ;; Text
              (2  . (fa_tags :face font-lock-function-name-face)) ;; Method
              (3  . (fa_tag  :face font-lock-function-name-face)) ;; Function
              (4  . (fa_tag  :face font-lock-function-name-face)) ;; Constructor
              (5  . (fa_cog  :foreground "#FF9800")) ;; Field
              (6  . (fa_cog  :foreground "#FF9800")) ;; Variable
              (7  . (fa_cube :foreground "#7C4DFF")) ;; Class
              (8  . (fa_cube :foreground "#7C4DFF")) ;; Interface
              (9  . (fa_cube :foreground "#7C4DFF")) ;; Module
              (10 . (fa_cog  :foreground "#FF9800")) ;; Property
              (11 . md_settings_system_daydream) ;; Unit
              (12 . (fa_cog  :foreground "#FF9800")) ;; Value
              (13 . (md_storage :face font-lock-type-face)) ;; Enum
              (14 . (md_closed_caption :foreground "#009688")) ;; Keyword
              (15 . md_closed_caption) ;; Snippet
              (16 . (md_color_lens :face font-lock-doc-face)) ;; Color
              (17 . fa_file_text_o) ;; File
              (18 . md_refresh) ;; Reference
              (19 . fa_folder_open) ;; Folder
              (20 . (md_closed_caption :foreground "#009688")) ;; EnumMember
              (21 . (fa_square :face font-lock-constant-face)) ;; Constant
              (22 . (fa_cube :face font-lock-type-face)) ;; Struct
              (23 . fa_calendar) ;; Event
              (24 . fa_square_o) ;; Operator
              (25 . fa_arrows)) ;; TypeParameter
            )
        ;(company-box-show-single-candidate t)
        ;(setq company-box-icons-alist 'company-box-icons-all-the-icons)
        ;(company-box-doc-delay 0.5)
)

(use-package lsp-mode :ensure t ;:after exec-path-from-shell
:commands lsp
:general (leader "hh" '(lsp-execute-code-action         :wk "wizard")
                 "pp" '(xref-go-back                    :wk "lsp pop")
                 "fd" '(lsp-ui-peek-find-definitions    :wk "lsp define")
                 "fi" '(lsp-ui-peek-find-implementation :wk "lsp impl")
                 "fr" '(lsp-ui-peek-find-references     :wk "lsp ref"))
:hook   (lsp-mode  . lsp-enable-which-key-integration)
:custom (lsp-inhibit-message t)
        (lsp-message-project-root-warning t)
        (lsp-enable-file-watchers nil)
        (lsp-enable-completion-at-point t)
        (lsp-prefer-flymake nil)
        (create-lockfiles nil)
        (make-backup-files nil)
        (lsp-file-watch-threshold nil)
        (lsp-response-timeout 25)
        (lsp-eldoc-render-all nil)
        ;(lsp-completion-provider :capf)
        (lsp-lens-enable t)
        (lsp-enable-snippet t)

        (lsp-rust-analyzer-server-display-inlay-hints nil)
        ;(lsp-rust-analyzer-cargo-watch-command "clipy")
:config
    ;(lsp-mode)
    (setq lsp-go-use-gofumpt t)
    (setq lsp-gopls-hover-kind "NoDocumentation")
    (lsp-register-custom-settings
        '(("gopls.staticcheck" t t)
          ("gopls.allExperiments" t t) 
          ("gopls.usePlaceholders" t t)
          ("rust-analyzer.cargo.runBuildScript" t t)
             ))
    (setq lsp-go-analyses
        '((unusedparams . t)
          (unreachable . t)
          (unusedwrite . t)
          (fieldalignment . t)
          (useany . t)))
    ;(setq lsp-enable-which-key-integration t)
    ;(setq lsp-go-gopls-placeholders nil)
)

(use-package lsp-ui :ensure t 
:commands lsp-ui-mode
:after  lsp-mode
:general (leader ;"ld"  #'lsp-ui-doc-focus-frame
                 "lpr" #'lsp-ui-peek-find-references
                 "lpd" #'lsp-ui-peek-find-definitions
                 "lpi" #'lsp-ui-peek-find-implementation)
         (:keymaps 'lsp-ui-peek-mode-map
                 "k"   #'lsp-ui-peek--select-prev
                 "j"   #'lsp-ui-peek--select-next)
:custom (scroll-margin 0)
        (lsp-headerline-breadcrumb-icons-enable t)
        (lsp-lens-enable nil)
        (lsp-ui-peek-enable t)
        (lsp-ui-flycheck-enable t)
        (lsp-ui-doc-enable t)
        (lsp-ui-doc-show-with-cursor t)
        (lsp-ui-sideline-enable t)
        (lsp-ui-sideline-show-hover nil)
        (lsp-ui-sideline-actions-icon nil)
        (lsp-ui-sideline-show-code-actions t)
        ;(lsp-ui-sideline-show-diagnostics t)
)

(use-package cov :ensure t)

(use-package editorconfig :ensure t)
(use-package copilot :load-path "lisp/copilot" :after editorconfig :disabled
 :config
      (add-hook 'prog-mode-hook 'copilot-mode)
      (customize-set-variable 'copilot-enable-predicates '(evil-insert-state-p))
    (defun my-copilot-complete ()
        (interactive)
        (or (copilot-accept-completion)
            (company-indent-or-complete-common nil)))
        ; modify company-mode behaviors
        (with-eval-after-load 'company
        ; disable inline previews
        (delq 'company-preview-if-just-one-frontend company-frontends)
            ; enable tab completion
            (define-key company-mode-map   (kbd "<C-tab>") 'my-copilot-complete)
            (define-key company-active-map (kbd "<C-return>") 'my-copilot-complete)
    )
)

(use-package treemacs :ensure t :config (setq treemacs-resize-icons 22))
(use-package treemacs-evil :ensure t :after (treemacs evil))
(use-package treemacs-projectile :ensure t :after (treemacs projectile))

(use-package flycheck :ensure t 
:custom (flycheck-clang-language-standard "c++17")
:config (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
        (global-flycheck-mode)
)

(use-package yasnippet :ensure t  
;https://github.com/joaotavora/yasnippet
:after (company)
:custom (yas-snippet-dirs '("~/.emacs.d/yas/"))
:general (leader  "hy"  '(:wk "Yasnippet")
                  "hyl" 'company-yasnippet)
:config (yas-global-mode t)
        (yas-reload-all t)
)

(use-package yasnippet-snippets :ensure t  :after yasnippet :defer t)
(use-package auto-yasnippet :ensure t 
;https://github.com/abo-abo/auto-yasnippet
:after yasnippet
:general (leader "hyc" 'aya-create
                 "hye" 'aya-expand)
)

(use-package cpp-mode ;:load-path "lisp/cpp-mode"
:no-require t
:ensure nil
:mode (("\\.h\\'"   . c++-mode)
       ("\\.hpp\\'" . c++-mode))
;:commands cpp-mode
:general (leader "hc" '(:wk "C/C++"))
;:hook (c-mode-common . 'cpp-mode)
:init (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;      (add-hook 'c++-mode-hook  'cpp-mode)
;      (add-hook 'c-mode-hook    'cpp-mode)
;      (add-hook 'objc-mode-hook 'cpp-mode)
)

(use-package ccls :ensure t ;:disabled; with lsp or eglot mode 
:hook  ((c-mode c++-mode objc-mode cuda-mode c-mode-common) . (lambda () (require 'ccls) (lsp)))
:config
    (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
    ;(setq ccls-sem-highlight-method 'font-lock)
    ;(ccls-use-default-rainbow-sem-highlight)
    (setq ccls-extra-init-params '(:client (:snippetSupport :json-false)))
    (setq ccls-executable "ccls")
    (setq ccls-initialization-options '(:compilationDatabaseDirectory "build/" ))
)

(use-package cppm :no-require t
:after c++-mode
:general (leader "hcb" (lambda () (eshell-command "cppm build"))
                 "hcr" (lambda () (eshell-command "cppm run  ")))
)

(use-package company-c-headers :ensure t 
:after  (company c++-mode)
:config (add-to-list 'company-backends 'company-c-headers)
)
(use-package clang-format :ensure t 
:after  (c++-mode)
:init   (add-hook 'c++-mode-hook 'clang-format)
:general (leader "hccf" 'clang-format-regieon)
)

(use-package lsp-treemacs :ensure t 
:after (lsp-mode doom-modeline)
:config ;(setq lsp-metals-treeview-enable t)
        ;(setq lsp-metals-treeview-show-when-views-received t)
        (lsp-treemacs-sync-mode 1)
) 

(use-package dap-mode :ensure t 
:after lsp-mode
:commands (dap-debug)
:general (leader "dd" 'dap-debug)
;:custom (dap-lldb-debug-program '("/Users/nieel/.vscode/extensions/lanza.lldb-vscode-0.2.2/bin/darwin/bin/lldb-vscode")) 
:config
    (setq dap-auto-configure-features '(sessions locals controls tooltip))
    (add-hook 'dap-stopped-hook (lambda (arg) (call-interactively #'dap-hydra)))
    ;(require 'dap-gdb-lldb) ; gdb mode
    (require 'dap-go)
    (dap-mode 1)
)

(use-package dap-ui-setting :no-require t
:after dap-mode
:preface
  (defun my/window-visible (b-name)
      "Return whether B-NAME is visible."
      (-> (-compose 'buffer-name 'window-buffer)
          (-map (window-list))
          (-contains? b-name)))

  (defun my/show-debug-windows (session)
      "Show debug windows."
      (let ((lsp--cur-workspace (dap--debug-session-workspace session)))
          (save-excursion
          ;; display locals
          (unless (my/window-visible dap-ui--locals-buffer)
              (dap-ui-locals))
          ;; display sessions
          (unless (my/window-visible dap-ui--sessions-buffer)
              (dap-ui-sessions)))))

  (defun my/hide-debug-windows (session)
      "Hide debug windows when all debug sessions are dead."
      (unless (-filter 'dap--session-running (dap--get-sessions))
          (and (get-buffer dap-ui--sessions-buffer)
              (kill-buffer dap-ui--sessions-buffer))
          (and (get-buffer dap-ui--locals-buffer)
              (kill-buffer dap-ui--locals-buffer))))
:config
    (add-hook 'dap-terminated-hook 'my/hide-debug-windows)
    (add-hook 'dap-stopped-hook 'my/show-debug-windows)
)

; only c/c++
(use-package disaster :ensure t  :commands disaster)

(use-package eldoc :ensure t  :diminish eldoc-mode :commands eldoc-mode)

(use-package emacs-lisp :no-require t
:general (leader "le" '(eval-print-last-sexp :wk "Elisp Evaluate"))
)

(use-package scratch-comment :ensure t 
:general (:keymaps 'lisp-interaction-mode-map "C-j" 'scratch-comment-eval-sexp)
)
  
(use-package slime :ensure t  :disabled
:commands slime
:config
    (setq inferior-lisp-program (or (executable-find "sbcl")
                                    (executable-find "/usr/bin/sbcl")
                                    (executable-find "/usr/sbin/sbcl" )))
    (require 'slime-autoloads)
    (slime-setup '(slime-fancy))
)
(use-package elisp-slime-nav :ensure t  :diminish elisp-slime-nav-mode
:hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode)
)

(use-package prettify-symbols :no-require t
:hook ((emacs-lisp-mode lisp-mode org-mode) . prettify-symbols-mode)
)

(use-package tree-sitter :ensure t :disabled
:config (tree-sitter-hl-mode)
        (global-tree-sitter-mode)
)
(use-package tree-sitter-langs :ensure t :after tree-sitter)
;(use-package tree-sitter-indent :ensure t)

(use-package csharp-mode :ensure t
:mode (("\\.cs\\'" . csharp-mode))
       ;("\\.cs\\'" . csharp-tree-sitter-mode))
:hook (csharp-mode . lsp)
)

(use-package rustic :ensure t
:ensure-system-package (rustup . "curl https://sh.rustup.rs -sSf | sh")
:mode ("\\.rs\\'" . rustic-mode)
:general (leader "hrf" 'rust-format-buffer)
:init
(defun ij/rustic-mode-hook ()
    (when buffer-file-name (setq-local buffer-save-without-query t)))

:config
    (setq lsp-eldoc-hook nil)
    (setq lsp-enable-symbol-highlighting nil)
    (setq lsp-signature-auto-activate nil)
    (setq rustic-lsp-server 'rust-analyzer)
    (lsp-flycheck-add-mode 'rustic-mode)
    (setq lsp-rust-server 'rust-analyzer)
    ;(setq lsp-rust-analyzer-cargo-watch-enable nil) ;; large project에서 cargo crate를 check하는것을 방지
    ;(setq rustic-format-on-save t)
    (add-hook 'rustic-mode-hook 'ij/rustic-mode-hook)
)




;(use-package rust-mode :ensure t 
;:ensure-system-package (rustup . "curl https://sh.rustup.rs -sSf | sh")
;:mode ("\\.rs\\'" . rust-mode)
;:hook (rust-mode . lsp)
;:general (leader "hrf" 'rust-format-buffer)
;:config  (setq lsp-rust-rls-command '("rustup", "run", "nightly", "rls"))
;         (setq lsp-rust-server 'rust-analyzer)
;         (setq lsp-rust-analyzer-cargo-watch-enable nil) ;; large project에서 cargo crate를 check하는것을 방지
;         ;(lsp-rust-analyzer-inlay-hints-mode t) ; display type hint 
;         ;(setq rust-format-on-save t)
;         ;(add-hook 'rust-mode-hook (lambda () (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))
;)

(use-package flycheck-rust :ensure t 
:after  (flycheck rust-mode)
:config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
)

(use-package cargo :ensure t 
:after  rust-mode
:hook (rust-mode . cargo-minor-mode)
:commands cargo-minor-mode
:general (leader "hrb" 'cargo-process-build
                 "hrr" 'cargo-process-run
                 "hrt" 'cargo-process-test)
)

(use-package haskell-mode :ensure t
:mode ("\\.hs\\'"    . haskell-mode)
)

(use-package lsp-haskell :ensure t :after haskell-mode
:hook ((haskell-mode . (lambda () (lsp)))
       (haskell-literate-mode . (lambda () (lsp))))
)

(use-package yaml-mode :ensure t 
:mode (("\\.yaml\\'" . yaml-mode)
       ("\\.yml\\'"  . yaml-mode))
)

(use-package toml-mode :ensure t 
:mode (("\\.toml\\'" . toml-mode)
       ("Pipfile\\'" . toml-mode))
)

(use-package cmake-mode :ensure t 
:ensure-system-package (cmake-language-server . "pip3 install cmake-language-server")
:commands cmake-mode
:mode (("\\.cmake\\'"    . cmake-mode)
       ("CMakeLists.txt" . cmake-mode))
:hook (cmake-mode . (lambda () (require 'lsp-cmake) (lsp)))
:init (setq cmake-tab-width 4)
)

(use-package poly-markdown :ensure t :disabled
;:after (markdown-mode polymode)
:hook (markdown-mode . poly-markdown-mode)
;:init (evil-set-initial-state 'poly-org-mode 'normal)
)

(use-package markdown-mode :ensure t 
:after poly-markdown
:mode  (("\\README.md\\'" . gfm-mode)
        ("\\.md\\'"       . gfm-mode)
        ("\\.markdown\\'" . gfm-mode))
:general (leader "hm" '(:wk "Markdown"))
:config (setq markdown-command "multimarkdown")
        (poly-markdown-mode)
)

(use-package markdown-preview-mode :ensure t  :defer t)
(use-package gh-md :ensure t  :defer t
:general (leader "hmr" 'gh-md-render-buffer)
)

(use-package python-mode :ensure t
:mode (("\\.py\\'" . python-mode)
       ("\\.wsgi$" . python-mode))
:interpreter (("python" . python-mode))
:ensure-system-package (;(pyenv . "")
                        (pipenv . "pip install pipenv"))
:custom (python-indent-offset 4)
)

(use-package pipenv :ensure t
:after (pyvenv-mode python-mode)
:hook (python-mode . pipenv-mode)
:config (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended)
)

(use-package lsp-pyright :ensure t 
:hook (python-mode . (lambda () (require 'lsp-pyright) (lsp)))
)

;(use-package lsp-python-ms :ensure t
;:hook (python-mode . (lambda () (require 'lsp-python-ms) (lsp)))
;:init (setq lsp-python-ms-auto-install-server t)
;      (setq lsp-python-ms-executable "~/.emacs.d/var/lsp-python-ms/Microsoft.Python.LanguageServer")
;)

(use-package dart-mode :ensure t 
:after lsp
:mode   ("\\.dart\\'" . dart-mode)
:custom (dart-format-on-save t)
        (dart-enable-analysis-server t)
        (dart-sdk-path (expand-file-name "~/dev/flutter/bin/cache/dart-sdk/"))
:init (add-hook 'dart-mode-hook 'lsp)
)

(use-package flutter :ensure t 
:after dart-mode
:general (:keymaps 'dart-mode-map "C-M-x" 'flutter-run-or-hot-reload)
:custom (flutter-sdk-path (expand-file-name "~/dev/flutter/"))
)

(use-package company-shell :ensure t
:after (company eshell)
:init (add-to-list 'company-backends '(company-shell company-shell-env company-fish-shell))
)

(use-package dotenv-mode :ensure t 
:mode ("\\.env\\..*\\'" . dotenv-mode)
)

(use-package powershell :ensure t)

(use-package go-mode :ensure t 
:ensure-system-package ((gopls . "go install golang.org/x/tools/gopls@latest")
                        (godef . "go install github.com/rogpeppe/godef@latest")
                        (gofumpt . "go install mvdan.cc/gofumpt@latest"))
             
:mode ("\\.go\\''" . go-mode)
:hook (go-mode . (lambda () (lsp)))
:config 
    ;(setq gofmt-command "goimports-reviser")
    ;(add-hook 'before-save-hook 'gofmt-before-save)
    (defun lsp-go-install-save-hooks ()
        (add-hook 'before-save-hook #'lsp-format-buffer)
        (add-hook 'before-save-hook #'lsp-organize-imports))
    (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
)

(use-package dap-go :ensure dap-mode :after go-mode :disabled
:config (dap-go-setup)
)

;:go-tag-add xml db
;go-tag-add json,omitempty
(use-package go-tag :ensure t :after go-mode
:ensure-system-package (gomodifytags . "go install github.com/fatih/gomodifytags@latest")
)

(use-package go-impl :load-path "lisp/go-impl" :after go-mode
:ensure-system-package ((impl . "go install github.com/josharian/impl@latest")
                        (godoc . "go install golang.org/x/tools/cmd/godoc@latest"))
)

(use-package go-fill-struct :ensure t :after go-mode
:ensure-system-package (fillstruct . "go install github.com/davidrjenni/reftools/cmd/fillstruct@latest")
)

(use-package go-gen-test :ensure t :after go-mode
:ensure-system-package (gotests . "go install github.com/cweill/gotests/...@latest")
)

(use-package gotest :ensure t :after go-mode)

(use-package go-errcheck :ensure t :after go-mode
:ensure-system-package (errcheck . "go install github.com/kisielk/errcheck@latest")
)

(defvar-local flycheck-local-checkers nil)
(defun +flycheck-checker-get(fn checker property)
    (or (alist-get property (alist-get checker flycheck-local-checkers))
        (funcall fn checker property)))
(advice-add 'flycheck-checker-get :around '+flycheck-checker-get)

(use-package flycheck-golangci-lint :ensure t :after (go-mode flycheck)
    :ensure-system-package ((golangci-lint . "curl -sSfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh | sh -s -- -b $(go env GOPATH)/bin v1.46.2")
                            (gocritic . "go install github.com/go-critic/go-critic/cmd/gocritic@latest")
                            (revive . "go install github.com/mgechev/revive@latest")
                            (staticcheck . "go install honnef.co/go/tools/cmd/staticcheck@latest"))
:config 
    (setq flycheck-golangci-lint-enable-linters '("gocritic" "misspell" "revive"))
    (setq flycheck-golangci-lint-disable-linters '("structcheck"))
    (add-hook 'go-mode-hook (lambda() (flycheck-golangci-lint-setup)
                                (setq flycheck-local-checkers '((lsp . ((next-checkers . (golangci-lint))))))))
)

(use-package dumb-jump :ensure t 
:after  company
:custom (dumb-jump-selector 'ivy)
        (dumb-jump-force-searcher 'rg)
        (dumb-jump-default-project "~/build")
:general (leader "hd"  '(:wk "Definition Jump")
                 "hdo" 'find-file-other-window
                 "hdj" 'xref-pop-marker-stack)
:init (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
)

(use-package web-mode :ensure t 
:ensure-system-package (nvm . "curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash")
:commands (web-mode)
:mode    (("\\.html?\\'"  . web-mode)
          ("\\.xhtml$\\'" . web-mode)
          ("\\.tsx$"      . web-mode)
          ("\\.vue\\'"    . web-mode))
:custom (web-mode-enable-engine-detection t)
        (web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
)

(use-package js2-mode :ensure t 
:mode (("\\.js\\'"  . js2-mode)
       ("\\.mjs\\'" . js2-mode))
:hook (js2-mode . (lambda () (lsp)))
)

(use-package xref-js2 :ensure t 
:after (js2-mode xref)
:config (add-hook 'js2-mode-hook (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
)

(use-package skewer-mode :ensure t 
:after js2-mode
:hook  ((js2-mode  . skewer-mode)
        (css-mode  . skewer-css-mode)
        (html-mode . skewer-html-mode))
)

(use-package typescript-mode :ensure t 
:mode  (("\\.ts\\'"  . typescript-mode)
        ("\\.tsx\\'" . typescript-mode))
:hook (typescript-mode . (lambda () (lsp)))
)

(use-package tide :ensure t 
:after (typescript-mode company flycheck)
:hook  ((typescript-mode . tide-setup)
        (typescript-mode . tide-hl-identifier-mode)
        (before-save . tide-format-before-save))
)

(use-package json-mode :ensure t 
:mode  (("\\.json\\'"       . json-mode)
        ("/Pipfile.lock\\'" . json-mode))
)

(use-package json-reformat :ensure t 
:commands json-reformat-region
)

(use-package company-restclient :ensure t 
:after  (company restclient)
:config (add-to-list 'company-backends 'company-restclient)
)

(use-package ruby-mode :ensure t 
:mode "\\.rb\\'"
:mode "Rakefile\\'"
:mode "Gemfile\\'"
:mode "Berksfile\\'"
:mode "Vagrantfile\\'"
:interpreter "ruby"
:custom (ruby-indent-level 4)
        (ruby-indent-tabs-mode nil)
)

(use-package rvm :ensure t 
:after ruby-mode
:ensure-system-package (rvm . "curl -sSL https://get.rvm.io | bash -s stable")
:config (rvm-use-default)
)

(use-package yari :ensure t  :after ruby-mode)

(use-package rubocop :ensure t
:ensure-system-package (rubocop . "sudo gem install rubocop")
:after ruby-mode
:init (add-hook 'ruby-mode-hook 'rubocop-mode)
)

(use-package robe :ensure t
:after (ruby-mode company)
:ensure-system-package (pry . "sudo gem install pry pry-doc")
:init (add-hook 'ruby-mode-hook 'robe-mode)
:config (push 'company-robe company-backends)
)

(use-package ruby-tools :ensure t 
:after ruby-mode
:init (add-hook 'ruby-mode-hook 'ruby-tools-mode)
)

(use-package gdscript-mode :ensure t :disabled
:hook   (gdscript-mode . lsp)
:custom (gdscript-godot-executable "/usr/local/Caskroom/godot/3.2.2/Godot.app/Contents/MacOS/Godot")
)
;(use-package company-godot-gdscript :quelpa (company-godot-gdscript :fetcher github :repo "francogarcia/company-godot-gdscript.el") :disabled
;:after (gdscript-mode company)
;:config (add-to-list 'company-backends 'company-godot-gdscript)
;)

;(use-package lsp-java :ensure t  :config (add-hook 'java-mode-hook 'lsp))
;(use-package dap-java :ensure nil)
(use-package gradle-mode :ensure t  :config (add-hook 'java-mode-hook 'gradle-mode))
(use-package groovy-mode :ensure t  
:mode (".gradle\\'" . groovy-mode)
)

(use-package kotlin-mode :ensure t 
:config
    (setq lsp-clients-kotlin-server-executable "~/dev/tools/kotlin-language-server/server/build/install/server/bin/kotlin-language-server")
    (add-hook 'kotlin-mode-hook 'lsp)
)

(use-package lsp-grammarly :ensure t :disabled
:hook (text-mode . (lambda () (require 'lsp-grammarly) (lsp)))
)

(use-package protobuf-mode :ensure t)

(use-package direnv :ensure t :config (direnv-mode))

(use-package graphql-mode :ensure t
:mode ((".graphql\\'" . graphql-mode)
       (".prisma\\'"  . graphql-mode))
:hook (graphql-mode . (lambda () (require 'lsp-graphql) (lsp)))
)

(use-package quickrun :ensure t
:general (leader "qr" #'quickrun)
:config
    (quickrun-add-command "c++/c1z"
        '((:command . "cppm")
          (:exec "%c build")
           :defualt "c++"))
)

(use-package terraform-mode :ensure t
    :ensure-system-package (terraform-ls . "go install github.com/hashicorp/terraform-ls@latest")
    :mode   ("\\.tf\\'" . terraform-mode)
    :config (setq terraform-indent-level 4)
            (setq lsp-terraform-enable-logging t)
            (lsp-register-client
                (make-lsp-client
                    :new-connection (lsp-stdio-connection '("~/go/bin/terraform-ls" "serve"))
                    :major-modes    '(terraform-mode)
                    :server-id      'terraform-ls)))
           (add-hook 'terraform-mode-hook 'lsp)

(use-package lua-mode :ensure t
:mode ("\\.lua\\'" . lua-mode)
)

; brew install rust base system command
(use-package rust-system-command :no-require t :ensure nil
:ensure-system-package ((rg    . "cargo install ripgrep")
                        (exa   . "cargo install exa")
                        (bat   . "cargo install bat")
                        (procs . "cargo install procs")
                        (dust  . "cargo install du-dust")
                        (ytop  . "cargo install ytop"))
)

; brew cask install karabiner-elements
(use-package karabiner :no-require t :ensure nil
:if *is-mac*
)
