(add-to-list 'load-path "~/.emacs.d/lisp/")
(setq-default custom-file (expand-file-name ".config.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

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

(setq backup-inhibited t)
(setq auto-save-default nil)
(setq make-backup-files nil)

(set-frame-parameter nil 'alpha 0.95)
(setq compilation-window-height 15)
(set-variable 'cursor-type '(hbar . 10))

;; No popup frame
(setq pop-up-frames nil)
(setq ring-bell-function 'ignore)
; layout save setting
(winner-mode t)
;(desktop-save-mode 1)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-splash-screen t)
(setq echo-keystrokes 0.5)
(setq global-hl-line-mode +1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; +------------+------------+
;; | 일이삼사오 | 일이삼사오 |
;; |------------+------------|
;; | 1234567890 | 1234567890 |
;; +------------+------------+
;; text utf-8 setting
(set-language-environment "Korean")
(prefer-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(set-face-attribute   'default nil       :family "DejaVu Sans Mono" :height 110)
(set-fontset-font nil 'hangul (font-spec :family "D2Coding" :pixelsize 18))
(setq face-font-rescale-alist '(("D2coding" . 1.2)))
(setq-default line-spacing 3)
(global-font-lock-mode t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(use-package which-key :ensure t 
:init   (which-key-mode t) 
:config (which-key-enable-god-mode-support t))

(use-package indent-guide :ensure t
:init (indent-guide-global-mode)
:config
    (setq indent-guide-char      "|")
    (setq indent-guide-recursive t)
    ;(set-face-background 'indent-guide-face "dimgray")
    ;(setq indent-guide-delay     0.1)
)
(defun my-set-indent (n)
    (setq-default tab-width n)
    (electric-indent-mode t)
    (setq c-basic-offset n)
    (setq lisp-indent-offset n)
    (setq indent-line-function 'insert-tab)
)
(my-set-indent 4)
(setq-default indent-tabs-mode nil)

(global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)
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
;(use-package highlight-indent-guides :ensure t
;    :init (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;    :config 
;       (setq highlight-indent-guides-method 'character)
;       ;(set-face-background 'highlight-indent-guides-odd-face       "darkgray")
;       ;(set-face-background 'highlight-indent-guides-even-face      "dimgray" )
;       ;(set-face-background 'highlight-indent-guides-character-face "dimgray" )
;)

(load-library "hideshow")
    (global-set-key (kbd "<C-right>") 'hs-show-block)
    (global-set-key (kbd "<C-left>")  'hs-hide-block)
    (add-hook 'c-mode-common-hook     'hs-minor-mode)
    (add-hook 'emacs-lisp-mode-hook   'hs-minor-mode)
    (add-hook 'java-mode-hook         'hs-minor-mode)
    (add-hook 'lisp-mode-hook         'hs-minor-mode)
    (add-hook 'perl-mode-hook         'hs-minor-mode)
    (add-hook 'sh-mode-hook           'hs-minor-mode)

(use-package evil :ensure t
    :init (evil-mode t)
    :config
    (setq evil-want-C-u-scroll t)
    (evil-set-initial-state 'calender-mode    'emacs)
    (evil-set-initial-state 'calculater-mode  'emacs)
    (evil-set-initial-state 'git-rebase-mode  'emacs)
    (evil-set-initial-state 'magit-blame-mode 'emacs)
    (setq-default evil-symbol-word-search t)
)
(use-package evil-leader :ensure t :defer t
:init (global-evil-leader-mode t)
:config
    (setq evil-leader/leader "<SPC>")
    (evil-leader/set-key
        "<SPC>" 'helm-M-x
        "er"    'eval-buffer
        "b"     'switch-to-buffer
        "f"     'find-file
        "t"     'eshell
        "ef"    (lambda ()(interactive)(find-file "~/.emacs.d/config.org"))
        "wh"    'shrink-window-horizontally
        "wj"    'enlarge-window
        "wk"    'shrink-window
        "wl"    'enlarge-window-horizontally
    )
)

(use-package beacon :ensure t :init (beacon-mode t)) 
(use-package git-gutter :ensure t
:init 
    (setq-default display-line-numbers-width 2)
    (global-git-gutter-mode t)
    (global-display-line-numbers-mode t)
    (global-hl-line-mode t)
:config
    (setq git-gutter:lighter " gg")
    (setq git-gutter:window-width 1)
    (setq git-gutter:modified-sign ".")
    (setq git-gutter:added-sign    "+")
    (setq git-gutter:deleted-sign  "-")
    (set-face-foreground 'git-gutter:added    "#daefa3")
    (set-face-foreground 'git-gutter:deleted  "#FA8072")
    (set-face-foreground 'git-gutter:modified "#b18cce")
)

(use-package doom-themes
:init (load-theme 'doom-one t)
:config
    (doom-themes-neotree-config)
    (doom-themes-org-config)
)

(use-package all-the-icons :ensure t)
(use-package spaceline :ensure t)
(use-package spaceline-config :ensure spaceline
:init (spaceline-spacemacs-theme)
:config
    (custom-set-faces '(mode-line-buffer-id ((t nil)))) ;; blend well with tango-dark
    (setq powerline-default-separator 'arrow)   ;; bar arrow wave utf-8
    (spaceline-toggle-buffer-id-on)
    (spaceline-toggle-input-method-on)
    (spaceline-toggle-buffer-modified-on)
    (spaceline-toggle-buffer-encoding-on)
    (spaceline-toggle-process-on)
    (spaceline-toggle-projectile-root-on)
    (spaceline-toggle-version-control-on)
    (spaceline-toggle-flycheck-error-on)
    (spaceline-toggle-flycheck-info-on)
    (spaceline-toggle-flycheck-warning-on)
    (spaceline-toggle-major-mode-on)
    (spaceline-toggle-minor-modes-on)
    (spaceline-toggle-line-column-on)
    (spaceline-toggle-window-number-on)
    (spaceline-toggle-buffer-encoding-on)
    (spaceline-toggle-evil-state-on)
    (spaceline-toggle-nyan-cat-on)
    (spaceline-helm-mode 1)
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
    (setq evil-normal-state-tag   (propertize "COMMAND "))
    (setq evil-emacs-state-tag    (propertize "EMACS   "))
    (setq evil-insert-state-tag   (propertize "INSERT  "))
    (setq evil-replace-state-tag  (propertize "REPLACE "))
    (setq evil-motion-state-tag   (propertize "MOTION  "))
    (setq evil-visual-state-tag   (propertize "VISUAL  "))
    (setq evil-operator-state-tag (propertize "OPERATE "))
)

;(use-package spaceline-all-the-icons :ensure t
;    :after spaceline
;    :config
;    ;(spaceline-all-the-icons-theme)
;    ;(spaceline-all-the-icons--setup-package-updates)
;    ;(spaceline-all-the-icons--setup-neotree)
;    ;(spaceline-all-the-icons--setup-git-ahead)
;    ;(spaceline-toggle-all-the-icons-eyebrowse-workspace-on)
;    ;(spaceline-toggle-all-the-icons-sunrise-on)
;    ;(spaceline-toggle-all-the-icons-sunset-on)
;    ;(spaceline-toggle-all-the-icons-time-on)
;    ;(spaceline-toggle-all-the-icons-weather-on)
;    ;(spaceline-toggle-all-the-icons-flycheck-status-on)
;    ;(spaceline-toggle-all-the-icons-flycheck-status-info-on)
;    ;(spaceline-toggle-all-the-icons-buffer-id-on)
;    ;(spaceline-toggle-all-the-icons-git-status-on)
;    ;(spaceline-toggle-all-the-icons-nyan-cat-on)
;    ;(spaceline-toggle-all-the-icons-narrowed-on)
;    ;(spaceline-toggle-all-the-icons-git-ahead-on)
;    ;(spaceline-toggle-all-the-icons-bookmark-on)
;    ;(spaceline-toggle-all-the-icons-projectile-on)
;    ;(spaceline-toggle-all-the-icons-window-number-on)
;    ;(spaceline-toggle-all-the-icons-mode-icon-on)
;    ;(spaceline-toggle-all-the-icons-battery-status-on)
;    ;(setq inhibit-compacting-font-caches t)
;)

(use-package nyan-mode :ensure t
:init (nyan-mode)
:config
    (setq-default nyan-wavy-trail t)
    (nyan-start-animation)
    (nyan-refresh)
)
(when window-system
    (use-package mode-icons :ensure t
    :init   (mode-icons-mode)
    :config (setq mode-icons-change-mode-name nil)
    )
)
(use-package fancy-battery :ensure t
:init   (fancy-battery-mode)
:config (setq fancy-battery-show-percentage t)
)

(use-package helm :defer t :ensure t :diminish helm-mode
:bind ("M-x" . helm-M-x)
:init (helm-mode 1)
;; helm always bottom
(add-to-list 'display-buffer-alist
            `(,(rx bos "*helm" (* not-newline) "*" eos)
                    (display-buffer-in-side-window)
                    (inhibit-same-window . t)
                    (window-height . 0.4)))
)
(use-package helm-projectile :ensure t 
:after projectile
:init (helm-projectile-on)
)
(use-package helm-company :ensure t
:after helm company
:init
    (define-key company-mode-map   (kbd "C-q") 'helm-company)
    (define-key company-active-map (kbd "C-q") 'helm-company)
)
(use-package helm-descbinds :ensure t 
:after helm
:init (helm-descbinds-mode)
)

(use-package paren :ensure t 
:init   (show-paren-mode 1)
:config (setq show-paren-delay 0)
)

(use-package rainbow-delimiters :ensure t
:hook ((prog-mode text-mode) . rainbow-delimiters-mode)
)

(use-package smartparens :ensure t
:init (smartparens-global-mode)
:config 
    (use-package evil-smartparens :ensure t
    :init (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))
)

(use-package projectile :defer t :ensure t
:init (projectile-mode t)
:config (evil-leader/set-key "p" 'projectile-command-map)
)

(use-package neotree :ensure t
:init 
    ;(add-hook 'neotree-mode-hook (lambda () (linum-mode nil)))
    (add-hook 'neotree-mode-hook 
        (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
            (define-key evil-normal-state-local-map (kbd "q")   'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "g")   'neotree-refresh)
            (define-key evil-normal-state-local-map (kbd "n")   'neotree-next-line)
            (define-key evil-normal-state-local-map (kbd "p")   'neotree-previous-line)
            (define-key evil-normal-state-local-map (kbd "A")   'neotree-stretch-toggle)
            (define-key evil-normal-state-local-map (kbd "H")   'neotree-hidden-file-toggle)
        ))
    (evil-leader/set-key "n" #'neotree-toggle);(lambda ()(interactive)((neotree-toggle)(linum-mode nil)))
:config
    (progn
        (setq-default neo-window-width 30)
        (setq-default neo-smart-open t)
        (setq-default neo-dont-be-alone t)
        (setq neo-force-change-root t)
        (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
    )
    (setq neo-show-hidden-files t)
    (setq projectile-switch-project-action 'neotree-projectile-action)
)

(use-package ace-window :ensure t
:init   (evil-leader/set-key "wo" 'ace-window)
:config (setq aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8))
)

(use-package eyebrowse :ensure t
:init (eyebrowse-mode t)
:config 
    (evil-leader/set-key
        "w;" 'eyebrowse-last-window-config
        "w0" 'eyebrowse-close-window-config
        "w1" 'eyebrowse-switch-to-window-config-1
        "w2" 'eyebrowse-switch-to-window-config-2
        "w3" 'eyebrowse-switch-to-window-config-3
        "w4" 'eyebrowse-switch-to-window-config-4
    )
)

(setq gdb-show-main t)
(evil-leader/set-key "gb" 'gud-break)
(evil-leader/set-key "gn" 'gud-next)
(evil-leader/set-key "gi" 'gud-step)
(evil-leader/set-key "gf" 'gud-finish)
(evil-leader/set-key "gt" '(lambda () (call-interactively 'gud-tbreak)
                                      (call-interactively 'gud-cont  )))

(use-package company :ensure t
:init (global-company-mode 1)
:config 
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 1)
    (setq company-show-numbers t)
    (define-key company-active-map (kbd "M-n") 0)
    (define-key company-active-map (kbd "M-p") 0)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
)
(with-eval-after-load 'company
    (add-hook 'c++-mode-hook        'company-mode)
    (add-hook 'c-mode-hook          'company-mode)
    (add-hook 'racer-mode-hook      'company-mode)
    (add-hook 'emacs-lisp-mode-hook 'company-mode)
    (add-hook 'org-mode-hook 'company-mode)
)

(use-package flycheck :ensure t 
:init (global-flycheck-mode t))
(use-package flycheck-pos-tip :ensure t 
:after flycheck
:init (flycheck-pos-tip-mode))

(use-package irony :ensure t 
:init 
    (add-hook 'c++-mode-hook   'irony-mode)
    (add-hook 'c-mode-hook     'irony-mode)
    (add-hook 'objc-mode-hook  'irony-mode)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
:config (setq irony-cdb-search-directory-list (quote ("." "build" "bin")))
)
(use-package company-irony :ensure t :after company
:init (add-to-list 'company-backends 'company-irony)
)
(use-package flycheck-irony :ensure t :after flycheck
:init (flycheck-irony-setup)
)
(use-package company-irony-c-headers :ensure t
:after company
:init (add-to-list 'company-backends '(company-irony-c-headers))
)
(use-package company-c-headers :ensure t
:after company
:init (add-to-list 'company-backends 'company-c-headers)
)
(use-package clang-format :ensure t
:init (global-set-key [C-M-tab] 'clang-format-regieon)
)

(use-package elisp-slime-nav :ensure t :diminish elisp-slime-nav-mode
:hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode)
;:init
;    (dolist (i '(emacs-lisp-mode-hook ielm-mode-hook))
;        (add-hook i 'elisp-slime-nav-mode)
;    )
)

(use-package magit :ensure t :diminish auto-revert-mode
:init   (evil-leader/set-key "gs" 'magit-status)
:config (setq vc-handled-backends nil)
)

(use-package undo-tree :ensure t :diminish undo-tree-mode
:bind (("C-u"   . undo-tree-undo)
       ("C-r"   . undo-tree-redo))
:init
    (global-undo-tree-mode)
    (defalias 'redo 'undo-tree-redo)
    (defalias 'undo 'undo-tree-undo)
)

(evil-leader/set-key "oe" 'org-edit-src-code)    
(evil-leader/set-key "ok" 'org-edit-src-exit)


